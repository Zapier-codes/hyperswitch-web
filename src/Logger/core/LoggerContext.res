type t = {
  sessionId: string,
  merchantId: string,
  paymentId: string,
}

let empty = {
  sessionId: "",
  merchantId: "",
  paymentId: "",
}

let context = ref(empty)

let onSessionChange = ref(_ => ())

let current = () => context.contents

let keep = (existing, incoming) =>
  switch incoming {
  | Some(value) =>
    switch value->String.trim {
    | "" => existing
    | value => value
    }
  | None => existing
  }

let setSessionData = (~sessionId=?, ~merchantId=?, ~paymentId=?, ()) => {
  let previous = context.contents

  let base = switch sessionId->Option.map(String.trim) {
  | Some(sessionId) if sessionId !== "" && sessionId !== previous.sessionId => {...empty, sessionId}
  | _ => previous
  }
  context := {
      sessionId: base.sessionId->keep(sessionId),
      merchantId: base.merchantId->keep(merchantId),
      paymentId: base.paymentId->keep(paymentId),
    }
  if context.contents.sessionId !== previous.sessionId {
    LoggerUtils.safeRun(() => onSessionChange.contents())
  }
  let current = context.contents
  if (
    current.sessionId !== previous.sessionId ||
    current.merchantId !== previous.merchantId ||
    current.paymentId !== previous.paymentId
  ) {
    LoggerUtils.safeRun(() =>
      LoggerQueue.backfillContext(
        ~sessionId=current.sessionId,
        ~merchantId=current.merchantId,
        ~paymentId=current.paymentId,
      )
    )
  }
}

let paymentIdOfClientSecret = clientSecret =>
  switch clientSecret->String.split("_secret_") {
  | parts if parts->Array.length >= 2 => parts->Array.get(0)->Option.getOr("")
  | _ => ""
  }

let paymentIdOfSdkAuthorization = sdkAuthorization =>
  try {
    sdkAuthorization
    ->Window.atob
    ->String.split(",")
    ->Array.findMap(entry => {
      let prefix = "payment_id="
      entry->String.startsWith(prefix)
        ? switch entry->String.sliceToEnd(~start=prefix->String.length) {
          | "" => None
          | value => Some(value)
          }
        : None
    })
  } catch {
  | _ => None
  }

let setPaymentIdFromCredentials = (~clientSecret="", ~sdkAuthorization=?) => {
  let paymentId = switch sdkAuthorization->Option.flatMap(paymentIdOfSdkAuthorization) {
  | Some(paymentId) => paymentId
  | None => clientSecret->paymentIdOfClientSecret
  }
  switch paymentId->String.trim {
  | "" => ()
  | paymentId => setSessionData(~paymentId, ())
  }
}

let setPaymentIdFromClientSecret = clientSecret =>
  switch clientSecret->paymentIdOfClientSecret->String.trim {
  | "" => ()
  | paymentId => setSessionData(~paymentId, ())
  }

let stringField = (source, key) =>
  source->Dict.get(key)->Option.flatMap(JSON.Decode.string)->Option.getOr("")

let nestedDict = (source, key) =>
  source->Dict.get(key)->Option.flatMap(JSON.Decode.object)->Option.getOr(Dict.make())

let readField = (message, key) =>
  [
    message,
    message->nestedDict("loggerContext"),
    message->nestedDict("metadata"),
    message->nestedDict("paymentOptions"),
    message->nestedDict("options"),
  ]
  ->Array.findMap(source =>
    switch source->stringField(key)->String.trim {
    | "" => None
    | value => Some(value)
    }
  )
  ->Option.getOr("")

let startSessionFromMessage = message =>
  LoggerUtils.safeRun(() => {
    let paymentId = switch message
    ->readField("sdkAuthorization")
    ->paymentIdOfSdkAuthorization {
    | Some(paymentId) => paymentId
    | None =>
      switch message->readField("paymentId") {
      | "" => message->readField("clientSecret")->paymentIdOfClientSecret
      | paymentId => paymentId
      }
    }
    setSessionData(
      ~sessionId=message->readField("sdkSessionId"),
      ~merchantId=message->readField("publishableKey"),
      ~paymentId,
      (),
    )
  })

let sharedContext = () => {
  let {sessionId, merchantId, paymentId} = context.contents
  (
    "loggerContext",
    [
      ("sdkSessionId", sessionId->JSON.Encode.string),
      ("publishableKey", merchantId->JSON.Encode.string),
      ("paymentId", paymentId->JSON.Encode.string),
    ]
    ->Dict.fromArray
    ->JSON.Encode.object,
  )
}
