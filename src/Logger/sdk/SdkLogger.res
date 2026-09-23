open LoggerTypes
include SdkLoggerEvents

let renderedAt = ref(None)

let logLifecycle = (
  ~event: lifecycleEvent,
  ~details=[],
  ~exn=?,
  ~failure=?,
  ~durationMs=?,
  ~paymentMethod=?,
  ~message=?,
) => {
  switch event {
  | AppRendered => renderedAt := Some(Date.now())
  | _ => ()
  }
  let durationMs = switch (event, durationMs) {
  | (PaymentAttempted, None) =>
    renderedAt.contents->Option.map(startedAt => Date.now() -. startedAt)
  | _ => durationMs
  }
  LoggerRuntime.emit(
    ~category=Lifecycle,
    ~spec=event->LoggerUtils.spec,
    ~severity=event->lifecycleSeverity,
    ~data=event->LoggerUtils.eventDetails,
    ~details,
    ~exn?,
    ~failure?,
    ~durationMs?,
    ~paymentMethod?,
    ~message?,
  )
}

let logState = (
  ~event: stateEvent,
  ~details=[],
  ~exn=?,
  ~failure=?,
  ~durationMs=?,
  ~paymentMethod=?,
  ~message=?,
) =>
  LoggerRuntime.emit(
    ~category=State,
    ~spec=event->LoggerUtils.spec,
    ~severity=event->stateSeverity,
    ~data=event->LoggerUtils.eventDetails,
    ~details,
    ~exn?,
    ~failure?,
    ~durationMs?,
    ~paymentMethod?,
    ~message?,
  )

let namedField = field => field->String.trim === "" ? "unnamed" : field

let identify = event =>
  switch event {
  | FieldEdited({field}) => FieldEdited({field: field->namedField})
  | FieldToggled({field, enabled}) => FieldToggled({field: field->namedField, enabled})
  | FieldFocused({field}) => FieldFocused({field: field->namedField})
  | FieldBlurred({field}) => FieldBlurred({field: field->namedField})
  | event => event
  }

let logUser = (~event: userEvent, ~details=[], ~paymentMethod=?, ~message=?) => {
  let event = event->identify
  switch (event, paymentMethod) {
  | (PaymentMethodSelected({method}), None) =>
    method->LoggerPaymentMethod.fromBackendValue->Option.forEach(LoggerContext.setPaymentMethod)
  | (PaymentMethodSelected(_), Some(paymentMethod))
  | (SavedMethodSelected(_), Some(paymentMethod)) =>
    LoggerContext.setPaymentMethod(paymentMethod)
  | _ => ()
  }
  let rateKey = switch event {
  | FieldEdited({field}) | FieldFocused({field}) | FieldBlurred({field}) => Some(field)
  | _ => None
  }
  LoggerRuntime.emit(
    ~category=User,
    ~spec=event->LoggerUtils.spec,
    ~severity=event->userSeverity,
    ~data=event->LoggerUtils.eventDetails,
    ~details,
    ~paymentMethod?,
    ~rateKey?,
    ~message?,
  )
}

let logCrash = (~origin: crashOrigin, ~exn=?, ~details=[], ~message=?) =>
  LoggerRuntime.emit(
    ~category=Crash,
    ~spec=origin->LoggerUtils.spec,
    ~severity=origin->crashSeverity,
    ~details,
    ~exn?,
    ~message?,
  )

let sdkOrigins = [GlobalVars.sdkUrl, GlobalVars.repoPublicPath]->Array.filterMap(value =>
  switch value->String.trim {
  | "" => None
  | value => Some(value)
  }
)

let isSdkFrame = source =>
  switch source->String.trim {
  | "" => false
  | source => sdkOrigins->Array.some(origin => source->String.includes(origin))
  }

let adoptSessionFromParent = (~paymentType) => {
  LoggerRuntime.configure(~source=Elements(paymentType))
  Window.addEventListener("message", (ev: Window.event) => {
    let message = try JSON.parseExn(ev.data) catch {
    | _ => JSON.Encode.null
    }
    message
    ->JSON.Decode.object
    ->Option.forEach(LoggerContext.startSessionFromMessage)
  })
}

let catchGlobalCrashes = (~ownsDocument) => {
  let reporting = ref(false)
  let report = (~origin, ~details) =>
    if !reporting.contents {
      reporting := true
      logCrash(~origin, ~details)
      reporting := false
    }

  let field = (json, key) =>
    json
    ->JSON.Decode.object
    ->Option.flatMap(object => object->Dict.get(key))
    ->Option.getOr(JSON.Encode.null)

  let text = (json, key) => json->field(key)->JSON.Decode.string->Option.getOr("")

  let describe = json =>
    switch json->JSON.Decode.string {
    | Some(text) => text
    | None => json->field("message")->JSON.Decode.string->Option.getOr("UNKNOWN")
    }

  let isOurs = source => ownsDocument || source->isSdkFrame

  let reportIfOurs = (~origin, ~message, ~source) =>
    if source->isOurs {
      report(
        ~origin,
        ~details=[
          ("error_message", message->JSON.Encode.string),
          ("error_source", source->JSON.Encode.string),
        ],
      )
    }

  Window.addEventListener("error", (event: JSON.t) =>
    reportIfOurs(
      ~origin=UncaughtError,
      ~message=event->field("message")->describe,
      ~source=event->text("filename"),
    )
  )

  Window.addEventListener("unhandledrejection", (event: JSON.t) => {
    let reason = event->field("reason")
    reportIfOurs(
      ~origin=UnhandledRejection,
      ~message=reason->describe,
      ~source=reason->text("stack"),
    )
  })
}

let observeApi = (
  ~event: apiEvent,
  ~url,
  ~details=[],
  ~timeoutMs=?,
  ~failureOf=LoggerUtils.httpFailure,
  ~detailsOf=LoggerUtils.httpDetails,
  ~paymentMethod=?,
  ~message=?,
  ~call,
) =>
  LoggerRuntime.observe(
    ~category=Api,
    ~spec=event->LoggerUtils.spec(~action=Request),
    ~severity=event->apiSeverity,
    ~data=event->LoggerUtils.eventDetails->Array.concat([("url", url->JSON.Encode.string)]),
    ~details,
    ~timeoutMs?,
    ~failureOf,
    ~detailsOf,
    ~paymentMethod?,
    ~message?,
    ~call,
  )

// Same as observeApi for calls that resolve to `(response, parsedBody)`, so the
// failure summary can read the error body, not just the status code.
let observeApiWithBody = (
  ~event: apiEvent,
  ~url,
  ~details=[],
  ~timeoutMs=?,
  ~paymentMethod=?,
  ~message=?,
  ~call,
) =>
  LoggerRuntime.observe(
    ~category=Api,
    ~spec=event->LoggerUtils.spec(~action=Request),
    ~severity=event->apiSeverity,
    ~data=event->LoggerUtils.eventDetails->Array.concat([("url", url->JSON.Encode.string)]),
    ~details,
    ~timeoutMs?,
    ~failureOf=LoggerUtils.httpBodyFailure,
    ~detailsOf=LoggerUtils.httpBodyDetails,
    ~paymentMethod?,
    ~message?,
    ~call,
  )

let observeStaticAsset = (
  ~event: staticAssetEvent,
  ~url,
  ~details=[],
  ~timeoutMs=?,
  ~failureOf=LoggerUtils.httpFailure,
  ~detailsOf=LoggerUtils.httpDetails,
  ~paymentMethod=?,
  ~message=?,
  ~call,
) =>
  LoggerRuntime.observe(
    ~category=Resource,
    ~spec=event->LoggerUtils.spec(~action=Load),
    ~severity=event->staticAssetSeverity,
    ~data=event
    ->LoggerUtils.eventDetails
    ->Array.concat([
      ("url", url->JSON.Encode.string),
      ("resource_type", "static_asset"->JSON.Encode.string),
    ]),
    ~details,
    ~timeoutMs?,
    ~failureOf,
    ~detailsOf,
    ~paymentMethod?,
    ~message?,
    ~call,
  )

let logApi = (
  ~event: apiEvent,
  ~outcome,
  ~details=[],
  ~startedAt=?,
  ~exn=?,
  ~paymentMethod=?,
  ~message=?,
) =>
  LoggerRuntime.emitPhase(
    ~category=Api,
    ~spec=event->LoggerUtils.spec(~action=Request),
    ~severity=event->apiSeverity,
    ~outcome,
    ~data=event->LoggerUtils.eventDetails,
    ~details,
    ~startedAt?,
    ~exn?,
    ~paymentMethod?,
    ~message?,
  )

let logFunction = (
  ~event: functionEvent,
  ~outcome,
  ~details=[],
  ~startedAt=?,
  ~exn=?,
  ~paymentMethod=?,
  ~message=?,
) =>
  LoggerRuntime.emitPhase(
    ~category=Function,
    ~spec=event->LoggerUtils.spec(~action=Call),
    ~severity=event->functionSeverity,
    ~outcome,
    ~data=event->LoggerUtils.eventDetails,
    ~details,
    ~startedAt?,
    ~exn?,
    ~paymentMethod?,
    ~message?,
  )

let observeFunction = (
  ~event: functionEvent,
  ~details=[],
  ~timeoutMs=?,
  ~failureOf=?,
  ~detailsOf=?,
  ~paymentMethod=?,
  ~message=?,
  ~call,
) =>
  LoggerRuntime.observe(
    ~category=Function,
    ~spec=event->LoggerUtils.spec(~action=Call),
    ~severity=event->functionSeverity,
    ~data=event->LoggerUtils.eventDetails,
    ~details,
    ~timeoutMs?,
    ~failureOf?,
    ~detailsOf?,
    ~paymentMethod?,
    ~message?,
    ~call,
  )

let observeFunctionCallback = (
  ~event: functionCallbackEvent,
  ~details=[],
  ~timeoutMs=?,
  ~failureOf=?,
  ~detailsOf=?,
  ~paymentMethod=?,
  ~message=?,
  ~callback,
) =>
  LoggerRuntime.observeCallback(
    ~category=Function,
    ~spec=event->LoggerUtils.spec(~action=Callback),
    ~severity=event->functionCallbackSeverity,
    ~data=event->LoggerUtils.eventDetails,
    ~details,
    ~timeoutMs?,
    ~failureOf?,
    ~detailsOf?,
    ~paymentMethod?,
    ~message?,
    ~callback,
  )

let observeResource = (
  ~event: resourceEvent,
  ~url,
  ~attributes=[],
  ~matchQuery=false,
  ~dedupe=true,
  ~paymentMethod=?,
  ~abandoned=() => false,
  ~message=?,
  ~onLoad=() => (),
  ~onError=_ => (),
) =>
  LoggerRuntime.observeResource(
    ~spec=event->LoggerUtils.spec(~action=Load),
    ~severity=event->resourceSeverity,
    ~url,
    ~resource=event->resourceKind,
    ~attributes,
    ~matchQuery,
    ~dedupe,
    ~paymentMethod?,
    ~abandoned,
    ~message?,
    ~onLoad,
    ~onError,
  )
