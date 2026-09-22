open LoggerTypes

type method =
  | Init
  | Reinit
  | Deinit
  | LoadHyper
  | CreateElements
  | CreateWidgets
  | Create
  | CreateCardForm
  | GetElement
  | Mount
  | Unmount
  | Destroy
  | Update
  | Clear
  | Collapse
  | Focus
  | Blur
  | ConfirmPayment
  | ConfirmCardPayment
  | ConfirmOneClickPayment
  | ConfirmWithCustomerDefaultPaymentMethod
  | ConfirmWithLastUsedPaymentMethod
  | RetrievePaymentIntent
  | PaymentRequest
  | InitPaymentSession
  | InitPaymentMethodSession
  | InitAuthenticationSession
  | PaymentMethodsManagementElements
  | GetCustomerSavedPaymentMethods
  | GetCustomerDefaultSavedPaymentMethodData
  | GetCustomerLastUsedPaymentMethodData
  | UpdateIntent
  | InitiateUpdateIntent
  | CompleteUpdateIntent
  | FetchUpdates
  | OnSdkHandleClick
  | On
  | Tokenize
  | ConfirmTokenization

type merchantCallEvent =
  | Hyper(method)
  | Elements(method)
  | PaymentElement(method)
  | CardForm(method)
  | CardField(method)
  | PaymentSession(method)
  | PaymentMethodsSession(method)

type merchantProp =
  | Appearance
  | Layout
  | Fields
  | Wallets
  | Terms
  | Business
  | DefaultValues
  | Branding
  | Locale
  | Loader
  | Fonts
  | CustomerPaymentMethods
  | PaymentMethodsConfig
  | PaymentElementOptions
  | PreloadSdkWithParams
  | TestMode
  | BlockConfirm
  | CustomPodUri
  | CustomBackendUrl
  | RedirectionFlags

type merchantPropEvent =
  | HyperProp(merchantProp)
  | ElementsProp(merchantProp)
  | PaymentElementProp(merchantProp)

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

let methodOf = event =>
  switch event {
  | Hyper(method)
  | Elements(method)
  | PaymentElement(method)
  | CardForm(method)
  | CardField(method)
  | PaymentSession(method)
  | PaymentMethodsSession(method) => method
  }

let merchantCallSpec = event =>
  makeOperation(
    call,
    `${event->LoggerUtils.variantName}_${event->methodOf->LoggerUtils.variantName}`,
  )

let merchantCallSeverity = event =>
  switch event->methodOf {
  | ConfirmPayment
  | ConfirmCardPayment
  | ConfirmOneClickPayment
  | ConfirmWithCustomerDefaultPaymentMethod
  | ConfirmWithLastUsedPaymentMethod
  | PaymentRequest
  | Tokenize
  | ConfirmTokenization => {success: Info, failure: Error}
  | _ => {success: Debug, failure: Error}
  }

let merchantPropSpec = event => {
  let (surface, merchantProp) = switch event {
  | HyperProp(merchantProp) => ("hyper", merchantProp)
  | ElementsProp(merchantProp) => ("elements", merchantProp)
  | PaymentElementProp(merchantProp) => ("payment_element", merchantProp)
  }
  {
    action: Some("prop"),
    subject: `${surface}_${merchantProp->LoggerUtils.variantName}`,
    outcome: None,
  }
}

let merchantPropSeverity = Debug

let merchantIssueSeverity = issue =>
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
    ~call,
  )

let logMerchantCall = (~event: merchantCallEvent, ~details=[]) =>
  LoggerRuntime.emit(
    ~category=Merchant,
    ~spec=event->merchantCallSpec->toEventSpec,
    ~severity=(event->merchantCallSeverity).success,
    ~data=event->LoggerUtils.eventDetails,
    ~details,
  )

let logMerchantProps = (~event: merchantPropEvent, ~details=[], ~paymentMethod=?) =>
  LoggerRuntime.emit(
    ~category=Merchant,
    ~spec=event->merchantPropSpec,
    ~severity=merchantPropSeverity,
    ~data=event->LoggerUtils.eventDetails,
    ~details,
    ~paymentMethod?,
    ~once=true,
  )

let logMerchantIssue = (~issue: merchantIssue, ~details=[]) =>
  LoggerRuntime.emit(
    ~category=Merchant,
    ~spec=issue->LoggerUtils.deriveFailure,
    ~severity=issue->merchantIssueSeverity,
    ~data=issue->LoggerUtils.eventDetails,
    ~details,
    ~once=true,
  )

let startSession = (~sessionId=?, ~merchantId=?, ~profileId=?) => {
  LoggerRuntime.configure(~source=HyperLoader)
  LoggerContext.setSessionData(~sessionId?, ~merchantId?, ~profileId?, ())
}
