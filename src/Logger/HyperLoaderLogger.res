open LoggerTypes

type surface =
  | Hyper
  | Elements
  | PaymentElement
  | CardForm
  | CardField
  | PaymentSession
  | PaymentMethodsSession

type surfaceData = {surface: surface}

type merchantCallEvent =
  | Init(surfaceData)
  | Reinit(surfaceData)
  | Deinit(surfaceData)
  | LoadHyper(surfaceData)
  | CreateElements(surfaceData)
  | CreateWidgets(surfaceData)
  | Create(surfaceData)
  | CreateCardForm(surfaceData)
  | GetElement(surfaceData)
  | Mount(surfaceData)
  | Unmount(surfaceData)
  | Destroy(surfaceData)
  | Update(surfaceData)
  | ConfirmPayment(surfaceData)
  | ConfirmCardPayment(surfaceData)
  | ConfirmOneClickPayment(surfaceData)
  | ConfirmWithCustomerDefaultPaymentMethod(surfaceData)
  | ConfirmWithLastUsedPaymentMethod(surfaceData)
  | RetrievePaymentIntent(surfaceData)
  | PaymentRequest(surfaceData)
  | InitPaymentSession(surfaceData)
  | InitPaymentMethodSession(surfaceData)
  | InitAuthenticationSession(surfaceData)
  | PaymentMethodsManagementElements(surfaceData)
  | GetCustomerSavedPaymentMethods(surfaceData)
  | GetCustomerDefaultSavedPaymentMethodData(surfaceData)
  | GetCustomerLastUsedPaymentMethodData(surfaceData)
  | UpdateIntent(surfaceData)
  | InitiateUpdateIntent(surfaceData)
  | CompleteUpdateIntent(surfaceData)
  | FetchUpdates(surfaceData)
  | Tokenize(surfaceData)
  | ConfirmTokenization(surfaceData)

type merchantPropEvent =
  | Appearance(surfaceData)
  | Locale(surfaceData)
  | Loader(surfaceData)
  | Fonts(surfaceData)
  | PaymentElementOptions(surfaceData)
  | PreloadSdkWithParams(surfaceData)
  | TestMode(surfaceData)
  | BlockConfirm(surfaceData)
  | CustomPodUri(surfaceData)
  | CustomBackendUrl(surfaceData)
  | RedirectionFlags(surfaceData)

type merchantCallbackEvent =
  | OnSdkHandleClick(surfaceData)
  | UpdateIntent(surfaceData)

type merchantIssue =
  | InvalidPublishableKey
  | InsecureProtocol
  | MissingParameter
  | MalformedValue
  | ImmutableAfterMount
  | ExpectedBoolean
  | ExpectedString
  | ExpectedNumber
  | ValueOutOfRange
  | ConnectorMisconfigured
  | UnsupportedOptionValue
  | UnknownOptionKey
  | DeprecatedMethod

let merchantCallSpec = event => makeOperation(call, event->LoggerUtils.variantName)

let merchantCallSeverity = event =>
  switch event {
  | ConfirmPayment(_)
  | ConfirmCardPayment(_)
  | ConfirmOneClickPayment(_)
  | ConfirmWithCustomerDefaultPaymentMethod(_)
  | ConfirmWithLastUsedPaymentMethod(_)
  | PaymentRequest(_)
  | Tokenize(_)
  | ConfirmTokenization(_) => {success: Info, failure: Error}
  | _ => {success: Debug, failure: Error}
  }

let merchantCallbackSpec = event => makeOperation(callback, event->LoggerUtils.variantName)

let merchantCallbackSeverity = event =>
  switch event {
  | OnSdkHandleClick(_)
  | UpdateIntent(_) => {success: Debug, failure: Warning}
  }

let merchantPropSpec = event => {
  action: Some("prop"),
  subject: event->LoggerUtils.variantName,
  outcome: None,
}

let merchantPropSeverity = Debug

let merchantIssueSeverity = (issue): severity =>
  switch issue {
  | InvalidPublishableKey
  | InsecureProtocol
  | MissingParameter
  | MalformedValue =>
    Error
  | ImmutableAfterMount
  | ExpectedBoolean
  | ExpectedString
  | ExpectedNumber
  | ValueOutOfRange
  | ConnectorMisconfigured
  | UnsupportedOptionValue
  | UnknownOptionKey
  | DeprecatedMethod =>
    Warning
  }

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
    ~spec=event->merchantCallSpec,
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
    ~spec=event->merchantCallbackSpec,
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
    ~spec=event->merchantCallSpec->toEventSpec(~outcome=Returned),
    ~severity=(event->merchantCallSeverity).success,
    ~data=event->LoggerUtils.eventDetails,
    ~details,
    ~message?,
  )

let logMerchantProps = (~event: merchantPropEvent, ~details=[], ~message=?) =>
  LoggerRuntime.emit(
    ~category=Merchant,
    ~spec=event->merchantPropSpec,
    ~severity=merchantPropSeverity,
    ~data=event->LoggerUtils.eventDetails,
    ~details,
    ~message?,
    ~once=true,
  )

let logMerchantIssue = (~issue: merchantIssue, ~details=[], ~message=?) =>
  LoggerRuntime.emit(
    ~category=Merchant,
    ~spec=issue->LoggerUtils.deriveFailure,
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
