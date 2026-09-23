open LoggerTypes
include HyperLoaderLoggerEvents

let observeMerchantCall = (
  ~event: merchantCallEvent,
  ~details=[],
  ~timeoutMs=?,
  ~failureOf=LoggerUtils.summarizeErrorResponse,
  ~detailsOf=?,
  ~message=?,
  ~call,
) =>
  LoggerRuntime.observe(
    ~category=Merchant,
    ~spec=event->LoggerGrammar.spec(~action=Call),
    ~severity=event->merchantCallSeverity,
    ~data=event->LoggerUtils.eventDetails,
    ~details,
    ~timeoutMs?,
    ~failureOf,
    ~detailsOf?,
    ~message?,
    ~call,
  )

let observeMerchantCallback = (
  ~event: merchantCallbackEvent,
  ~details=[],
  ~timeoutMs=?,
  ~failureOf=LoggerUtils.summarizeErrorResponse,
  ~detailsOf=?,
  ~message=?,
  ~callback,
) =>
  LoggerRuntime.observeCallback(
    ~category=Merchant,
    ~spec=event->LoggerGrammar.spec(~action=Callback),
    ~severity=event->merchantCallbackSeverity,
    ~data=event->LoggerUtils.eventDetails,
    ~details,
    ~timeoutMs?,
    ~failureOf,
    ~detailsOf?,
    ~message?,
    ~callback,
  )

let logMerchantCall = (~event: merchantCallEvent, ~details=[], ~message=?) =>
  LoggerRuntime.emit(
    ~category=Merchant,
    ~spec=event->LoggerGrammar.spec(~action=Call, ~outcome=Returned),
    ~severity=(event->merchantCallSeverity).success,
    ~data=event->LoggerUtils.eventDetails,
    ~details,
    ~message?,
  )

let logMerchantProps = (~event: merchantPropEvent, ~details=[], ~message=?) =>
  LoggerRuntime.emit(
    ~category=Merchant,
    ~spec=event->LoggerGrammar.spec(~action=Prop),
    ~severity=event->merchantPropSeverity,
    ~data=event->LoggerUtils.eventDetails,
    ~details,
    ~message?,
    ~once=true,
  )

let logMerchantIssue = (~issue: merchantIssue, ~details=[], ~message=?) =>
  LoggerRuntime.emit(
    ~category=Merchant,
    ~spec=issue->LoggerGrammar.spec(~action=IntegrationIssue),
    ~severity=issue->merchantIssueSeverity,
    ~data=issue->LoggerUtils.eventDetails,
    ~details,
    ~message?,
    ~once=true,
  )

let startSession = (~sessionId=?, ~merchantId=?, ~profileId=?) => {
  LoggerRuntime.configure(~source=HyperLoader)
  LoggerContext.setSessionData(~sessionId?, ~merchantId?, ~profileId?, ())
}
