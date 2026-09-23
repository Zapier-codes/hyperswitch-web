// How an event name is spelled. Every `event_name` in a log row is produced
// here and nowhere else:
//
//   <category>.<subject>                     Fact
//   <category>.<action>.<subject>            Prop / IntegrationIssue
//   <category>.<action>_<outcome>.<subject>  Call / Callback / Load / Request
//
// `subject` is the variant constructor of the typed event, snake_cased. The
// outcome (init/done/failed/...) is filled in by the runtime as the operation
// progresses; loggers only choose the action.

open LoggerTypes

// --- Casing ---

let snakeCase = value =>
  value
  ->String.replaceRegExp(/([A-Z]+)([A-Z][a-z])/g, "$1_$2")
  ->String.replaceRegExp(/([a-z0-9])([A-Z])/g, "$1_$2")
  ->String.toLowerCase

let screamingSnakeCase = value =>
  value
  ->snakeCase
  ->String.replaceRegExp(/[^a-zA-Z0-9]+/g, "_")
  ->String.replaceRegExp(/^_+|_+$/g, "")
  ->String.toUpperCase

let isVariantConstructor = value => value->String.match(/^[A-Z][A-Za-z0-9]*$/)->Option.isSome

// --- Variant constructor -> name ---

let variantConstructor = value => {
  let json = value->Identity.anyTypeToJson
  switch json->JSON.Decode.string {
  | Some(name) => name
  | None =>
    json
    ->JSON.Decode.object
    ->Option.flatMap(object => object->Dict.get("TAG"))
    ->Option.flatMap(JSON.Decode.string)
    ->Option.getOr("unknown")
  }
}

let variantName = value => value->variantConstructor->snakeCase

let variantValue = value => value->variantConstructor->screamingSnakeCase

// --- Typed event -> spec ---

// The one way to build a spec. Every logger calls this; the optional
// arguments pick the shape:
//
//   spec(QrCodeShown)                              -> <category>.qr_code_shown
//   spec(Locale, ~action=Prop)                     -> <category>.prop.locale
//   spec(LoadPaymentData, ~action=Call)            -> <category>.call_<outcome>.load_payment_data
//                                                      (outcome filled by the runtime per phase)
//   spec(Mount, ~action=Call, ~outcome=Returned)   -> <category>.call_returned.mount
//                                                      (single sync row, outcome fixed here)
let spec = (event, ~action=Fact, ~outcome=?): eventSpec => {
  action,
  subject: event->variantName,
  outcome,
}

// --- Assemble the final name ---

let eventName = (~category, ~action, ~subject, ~outcome) => {
  let step = switch (action->actionWord, outcome) {
  | (Some(action), Some(outcome)) => Some(`${action}_${outcome->outcomeName}`)
  | (Some(action), None) => Some(action)
  | (None, Some(outcome)) => Some(outcome->outcomeName)
  | (None, None) => None
  }
  [Some(category->categorySegment), step, Some(subject)]
  ->Array.filterMap(segment => segment)
  ->Array.join(".")
}
