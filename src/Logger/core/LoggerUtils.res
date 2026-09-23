open LoggerTypes

let maxTextLength = 256
let maxDetailBytes = 8192

let truncate = value =>
  value->String.length > maxTextLength ? value->String.slice(~start=0, ~end=maxTextLength) : value

let sanitizeUrl = url => url->String.replaceRegExp(/[?#].*$/, "")

let safeRun = action =>
  try action() catch {
  | error => Console.error2("hyper logging internals failed:", error)
  }

let stringOfBool = value => value ? "true" : "false"

let rec normalizeJson = json =>
  switch Type.Classify.classify(json) {
  | Undefined => JSON.Encode.null
  | _ =>
    switch JSON.Classify.classify(json) {
    | String(value) => value->truncate->JSON.Encode.string
    | Array(values) => values->Array.map(normalizeJson)->JSON.Encode.array
    | Object(object) =>
      object
      ->Dict.toArray
      ->Array.filterMap(((key, value)) =>
        switch Type.Classify.classify(value) {
        | Undefined => None
        | _ => {
            let key = key->LoggerGrammar.snakeCase
            Some((key, normalizeValue(key, value)))
          }
        }
      )
      ->Dict.fromArray
      ->JSON.Encode.object
    | _ => json
    }
  }
and normalizeValue = (key, value) =>
  switch (key, value->JSON.Decode.string) {
  | ("url" | "href" | "return_url", Some(text)) => text->sanitizeUrl->truncate->JSON.Encode.string
  | (_, Some(text)) if text->LoggerGrammar.isVariantConstructor =>
    text->LoggerGrammar.screamingSnakeCase->JSON.Encode.string
  | _ => value->normalizeJson
  }

let normalizeDetails = (entries: details): details =>
  entries->Array.map(((key, value)) => {
    let key = key->LoggerGrammar.snakeCase
    (key, normalizeValue(key, value))
  })

let eventDetails = (event): details =>
  switch event->Identity.anyTypeToJson->JSON.Decode.object {
  | None => []
  | Some(object) =>
    object
    ->Dict.get("_0")
    ->Option.flatMap(payload => payload->normalizeJson->JSON.Decode.object)
    ->Option.map(Dict.toArray)
    ->Option.getOr([])
  }

let mergeDetails = (~data: details, ~details: details): details => {
  let typedKeys = data->Array.map(((key, _)) => key)
  data->Array.concat(
    details->Array.map(((key, value)) =>
      typedKeys->Array.includes(key) ? ("detail_" ++ key, value) : (key, value)
    ),
  )
}

let fitToBudget = (entries: details): details => {
  let sizeOf = (entries: details) =>
    entries->Dict.fromArray->JSON.Encode.object->JSON.stringify->String.length
  let valueSize = ((_, value)) => value->JSON.stringify->String.length
  if entries->sizeOf <= maxDetailBytes {
    entries
  } else {
    let remaining = entries->Array.copy
    let dropped = []
    while remaining->Array.length > 0 && remaining->sizeOf > maxDetailBytes {
      let (largest, _) = remaining->Array.reduceWithIndex((0, -1), (
        (largest, largestSize),
        entry,
        index,
      ) => {
        let size = entry->valueSize
        size > largestSize ? (index, size) : (largest, largestSize)
      })
      remaining
      ->Array.get(largest)
      ->Option.forEach(((key, _)) => dropped->Array.push(key->JSON.Encode.string))
      remaining->Array.splice(~start=largest, ~remove=1, ~insert=[])
    }
    remaining->Array.concat([("dropped_details", dropped->JSON.Encode.array)])
  }
}

let maxPayloadFields = 120

let rec collectFields = (json, ~prefix, ~into) =>
  switch JSON.Classify.classify(json) {
  | Object(object) =>
    object
    ->Dict.toArray
    ->Array.forEach(((key, value)) => {
      let path =
        prefix === "" ? key->LoggerGrammar.snakeCase : prefix ++ "." ++ key->LoggerGrammar.snakeCase
      switch JSON.Classify.classify(value) {
      | Object(_) | Array(_) => value->collectFields(~prefix=path, ~into)
      | _ => into->Array.push(path)
      }
    })
  | Array(values) =>
    values->Array.forEach(value => value->collectFields(~prefix=prefix ++ "[]", ~into))
  | _ => prefix === "" ? () : into->Array.push(prefix)
  }

let payloadFields = body =>
  switch body->JSON.parseExn {
  | json => {
      let into = []
      json->collectFields(~prefix="", ~into)
      let unique =
        into->Array.reduce([], (acc, path) =>
          acc->Array.includes(path) ? acc : acc->Array.concat([path])
        )
      unique->Array.length > maxPayloadFields
        ? unique->Array.slice(~start=0, ~end=maxPayloadFields)
        : unique
    }
  | exception _ => []
  }

let payloadDetails = body =>
  switch body->payloadFields {
  | [] => []
  | fields => [
      ("request_fields", fields->Array.map(JSON.Encode.string)->JSON.Encode.array),
      ("request_field_count", fields->Array.length->JSON.Encode.int),
    ]
  }

let firstString = (object, keys) =>
  keys->Array.findMap(key => object->Dict.get(key)->Option.flatMap(JSON.Decode.string))

let summarizeValue = value => {
  let json = value->Identity.anyTypeToJson
  switch JSON.Classify.classify(json) {
  | String(value) => {name: "THROWN_VALUE", message: Some(value->truncate), details: []}
  | Object(object) => {
      name: object
      ->firstString(["name", "code", "type", "reason"])
      ->Option.getOr("UNKNOWN_ERROR")
      ->LoggerGrammar.screamingSnakeCase,
      message: object
      ->firstString(["message", "description", "statusMessage"])
      ->Option.map(truncate),
      details: [],
    }
  | _ => {name: "UNKNOWN_ERROR", message: None, details: []}
  }
}

let summarizeExn = error =>
  switch error {
  | Exn.Error(jsError) =>
    switch jsError->Exn.name {
    | Some(name) => {
        name: name->LoggerGrammar.screamingSnakeCase,
        message: jsError->Exn.message->Option.map(truncate),
        details: [],
      }
    | None => jsError->summarizeValue
    }
  | _ => error->summarizeValue
  }

let summarizeUnknown = value => value->Exn.anyToExnInternal->summarizeExn

let summarizeErrorResponse = result =>
  result
  ->Identity.anyTypeToJson
  ->JSON.Decode.object
  ->Option.flatMap(object => object->Dict.get("error"))
  ->Option.flatMap(json =>
    switch JSON.Classify.classify(json) {
    | Null => None
    | String(message) =>
      Some({name: "ERROR_RESPONSE", message: Some(message->truncate), details: []})
    | Object(object) =>
      Some({
        name: object
        ->firstString(["type", "code", "reason"])
        ->Option.getOr("ERROR_RESPONSE")
        ->LoggerGrammar.screamingSnakeCase,
        message: object->firstString(["message"])->Option.map(truncate),
        details: [
          ("error_code", object->firstString(["code"])),
          ("error_reason", object->firstString(["reason"])),
        ]->Array.filterMap(((key, value)) =>
          value->Option.map(value => (key, value->JSON.Encode.string))
        ),
      })
    | _ => Some({name: "ERROR_RESPONSE", message: None, details: []})
    }
  )

let httpFailure = response =>
  response->Fetch.Response.ok
    ? None
    : Some({
        name: "HTTP_ERROR",
        message: Some(response->Fetch.Response.status->Int.toString),
        details: [],
      })

let httpDetails = response => [("status_code", response->Fetch.Response.status->JSON.Encode.int)]

let httpBodyFailure = ((response, data)) =>
  response->Fetch.Response.ok
    ? None
    : Some(
        data
        ->summarizeErrorResponse
        ->Option.getOr({
          name: "HTTP_ERROR",
          message: Some(response->Fetch.Response.status->Int.toString),
          details: [],
        }),
      )

let httpBodyDetails = ((response, _)) => response->httpDetails

let intentErrorDetails = value =>
  switch value->Identity.anyTypeToJson->JSON.Decode.object {
  | None => []
  | Some(object) =>
    [
      ("error_message", object->firstString(["error_message"])),
      ("error_code", object->firstString(["error_code"])),
      ("error_reason", object->firstString(["error_reason"])),
    ]->Array.filterMap(((key, value)) =>
      value->Option.map(value => (key, value->truncate->JSON.Encode.string))
    )
  }

let errorDetails = summary =>
  [("error_type", summary.name->JSON.Encode.string)]
  ->Array.concat(
    summary.message
    ->Option.map(message => [("error_message", message->JSON.Encode.string)])
    ->Option.getOr([]),
  )
  ->Array.concat(summary.details)

let isAborted = summary => summary.name === "ABORT_ERROR"

let outcomeDetails = operationOutcome => {
  let duration =
    operationOutcome
    ->durationOf
    ->Option.map(durationMs => [("duration_ms", durationMs->JSON.Encode.float)])
    ->Option.getOr([])
  duration->Array.concat(
    switch operationOutcome {
    | OpStarted | OpDone(_) | OpReturned(_) | OpTriggered(_) | OpReused(_) => []
    | OpFailed({class, error}) =>
      [("failure_class", class->LoggerGrammar.variantValue->JSON.Encode.string)]->Array.concat(
        error->Option.map(errorDetails)->Option.getOr([]),
      )
    | OpTimedOut({timeoutMs}) => timeoutMs > 0 ? [("timeout_ms", timeoutMs->JSON.Encode.int)] : []
    },
  )
}

let outcomeSeverity = (operationOutcome, ~severity) =>
  switch operationOutcome {
  | OpFailed({error: Some(error)}) if error->isAborted => Debug
  | operationOutcome => severity->operationSeverityOf(~outcome=operationOutcome)
  }
