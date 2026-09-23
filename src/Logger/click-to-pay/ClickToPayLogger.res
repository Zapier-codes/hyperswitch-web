open LoggerTypes
include ClickToPayLoggerEvents

let paymentMethod = LoggerPaymentMethod.Card(Unspecified)

let logLifecycle = (~event: lifecycleEvent, ~details=[], ~exn=?, ~message=?) =>
  LoggerRuntime.emit(
    ~category=Lifecycle,
    ~spec=event->LoggerUtils.spec,
    ~severity=event->lifecycleSeverity,
    ~data=event->LoggerUtils.eventDetails,
    ~details,
    ~exn?,
    ~paymentMethod,
    ~message?,
  )

let observeFunction = (
  ~event: functionEvent,
  ~details=[],
  ~timeoutMs=?,
  ~detailsOf=?,
  ~paymentMethod=paymentMethod,
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
    ~failureOf=LoggerUtils.summarizeErrorResponse,
    ~detailsOf?,
    ~paymentMethod,
    ~message?,
    ~call,
  )

let observeMerchantCall = (
  ~method: merchantMethod,
  ~details=[],
  ~timeoutMs=?,
  ~detailsOf=?,
  ~paymentMethod=paymentMethod,
  ~message=?,
  ~call,
) =>
  LoggerRuntime.observe(
    ~category=Merchant,
    ~spec=method->LoggerUtils.spec(~action=Call),
    ~severity=method->merchantCallSeverity,
    ~data=[("surface", "AUTHENTICATION_SESSION"->JSON.Encode.string)],
    ~details,
    ~timeoutMs?,
    ~failureOf=LoggerUtils.summarizeErrorResponse,
    ~detailsOf?,
    ~paymentMethod,
    ~message?,
    ~call,
  )

let observeApi = (
  ~event: apiEvent,
  ~url,
  ~details=[],
  ~failureOf=LoggerUtils.httpFailure,
  ~detailsOf=LoggerUtils.httpDetails,
  ~message=?,
  ~call,
) =>
  LoggerRuntime.observe(
    ~category=Api,
    ~spec=event->LoggerUtils.spec(~action=Request),
    ~severity=event->apiSeverity,
    ~data=event->LoggerUtils.eventDetails->Array.concat([("url", url->JSON.Encode.string)]),
    ~details,
    ~failureOf,
    ~detailsOf,
    ~paymentMethod,
    ~message?,
    ~call,
  )

let observeResource = (
  ~event: resourceEvent,
  ~url,
  ~attributes=[],
  ~matchQuery=false,
  ~dedupe=true,
  ~timeoutMs=?,
  ~abandoned=?,
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
    ~timeoutMs?,
    ~abandoned?,
    ~paymentMethod,
    ~message?,
    ~onLoad,
    ~onError,
  )
