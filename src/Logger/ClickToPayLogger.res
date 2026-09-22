open LoggerTypes

type provider = LoggerTaxonomy.clickToPayProvider

type merchantMethod =
  | InitSession
  | GetActiveSession
  | IsCustomerPresent
  | GetUserType
  | GetRecognizedCards
  | ValidateAuthentication
  | CheckoutWithCard
  | SignOut

type apiEvent =
  | EnabledAuthnMethodsToken
  | EligibilityCheck
  | AuthenticationSync

type resourceEvent =
  | VisaSdkScript
  | MastercardSdkScript
  | UiKitScript
  | UiKitStylesheet

type providerDetails = {provider: provider}
type cardsData = {provider: provider, actionCode: string, visa: int, mastercard: int}
type declineData = {provider: provider, code: string}

type lifecycleEvent =
  | ProviderReady(providerDetails)
  | ProviderUnavailable(providerDetails)
  | RecognitionTokenFound(providerDetails)
  | CardsListed(cardsData)
  | CardsUnavailable(declineData)
  | CustomerVerificationRequired(providerDetails)
  | CustomerRecognised(providerDetails)
  | CheckoutCompleted(providerDetails)
  | CheckoutDeclined(declineData)
  | CheckoutCancelled(declineData)
  | CheckoutFailed(providerDetails)
  | OtpRejected(providerDetails)
  | PopupBlocked(providerDetails)

type functionEvent =
  | Initialize(providerDetails)
  | Init(providerDetails)
  | IdentityLookup(providerDetails)
  | GetCards(providerDetails)
  | Authenticate(providerDetails)
  | EncryptCard(providerDetails)
  | Checkout(providerDetails)
  | CheckoutWithCard(providerDetails)
  | CheckoutWithNewCard(providerDetails)
  | UnbindAppInstance(providerDetails)
  | SignOut(providerDetails)

let lifecycleSeverity = event =>
  switch event {
  | ProviderReady(_)
  | RecognitionTokenFound(_)
  | CheckoutCancelled(_)
  | CardsListed(_) =>
    Debug
  | CustomerVerificationRequired(_)
  | CustomerRecognised(_)
  | CheckoutCompleted(_)
  | CheckoutDeclined(_) =>
    Info
  | ProviderUnavailable(_)
  | CardsUnavailable(_)
  | OtpRejected(_) =>
    Warning
  | CheckoutFailed(_)
  | PopupBlocked(_) =>
    Error
  }

let payloadProvider = event =>
  switch event {
  | Initialize({provider})
  | Init({provider})
  | IdentityLookup({provider})
  | GetCards({provider})
  | Authenticate({provider})
  | EncryptCard({provider})
  | Checkout({provider})
  | CheckoutWithCard({provider})
  | CheckoutWithNewCard({provider})
  | UnbindAppInstance({provider})
  | SignOut({provider}) => provider
  }

let functionSpec = event => {
  let subject = `${event
    ->payloadProvider
    ->LoggerUtils.variantName}_${event->LoggerUtils.variantName}`
  makeOperation(call, subject)
}

let functionSeverity = event =>
  switch event {
  | Initialize(_)
  | Init(_)
  | IdentityLookup(_)
  | GetCards(_)
  | Authenticate(_)
  | UnbindAppInstance(_)
  | SignOut(_) => {success: Debug, failure: Warning}
  | EncryptCard(_)
  | Checkout(_)
  | CheckoutWithCard(_)
  | CheckoutWithNewCard(_) => {success: Debug, failure: Error}
  }

let merchantCallSpec = method => makeOperation(call, method->LoggerUtils.variantName)

let merchantCallSeverity = (method: merchantMethod) =>
  switch method {
  | CheckoutWithCard => {success: Info, failure: Error}
  | InitSession
  | GetActiveSession
  | IsCustomerPresent
  | GetUserType
  | GetRecognizedCards
  | ValidateAuthentication
  | SignOut => {success: Debug, failure: Warning}
  }

let apiSpec = value => makeOperation(request, value->LoggerUtils.variantName)

let apiSeverity = {success: Info, failure: Error}

let resourceSpec = value => makeOperation(load, value->LoggerUtils.variantName)

let resourceSeverity = {success: Debug, failure: Warning}

let resourceKind = (value): ResourceLoader.resource =>
  switch value {
  | UiKitStylesheet => Stylesheet
  | VisaSdkScript | MastercardSdkScript | UiKitScript => Script
  }

let paymentMethod = LoggerTaxonomy.Card(Unspecified)

let logLifecycle = (~event: lifecycleEvent, ~details=[], ~exn=?) =>
  LoggerRuntime.emit(
    ~category=Lifecycle,
    ~spec=event->LoggerUtils.deriveEvent,
    ~severity=event->lifecycleSeverity,
    ~data=event->LoggerUtils.eventDetails,
    ~details,
    ~exn?,
    ~paymentMethod,
  )

let observeFunction = (
  ~event: functionEvent,
  ~details=[],
  ~timeoutMs=?,
  ~detailsOf=?,
  ~paymentMethod=paymentMethod,
  ~call,
) =>
  LoggerRuntime.observe(
    ~category=Function,
    ~spec=event->functionSpec,
    ~severity=event->functionSeverity,
    ~data=event->LoggerUtils.eventDetails,
    ~details,
    ~timeoutMs?,
    ~failureOf=LoggerUtils.summarizeErrorResponse,
    ~detailsOf?,
    ~paymentMethod,
    ~call,
  )

let observeMerchantCall = (
  ~method: merchantMethod,
  ~details=[],
  ~timeoutMs=?,
  ~detailsOf=?,
  ~paymentMethod=paymentMethod,
  ~call,
) =>
  LoggerRuntime.observe(
    ~category=Merchant,
    ~spec=method->merchantCallSpec,
    ~severity=method->merchantCallSeverity,
    ~data=method->LoggerUtils.eventDetails,
    ~details,
    ~timeoutMs?,
    ~failureOf=LoggerUtils.summarizeErrorResponse,
    ~detailsOf?,
    ~paymentMethod,
    ~call,
  )

let observeApi = (
  ~event: apiEvent,
  ~url,
  ~details=[],
  ~failureOf=LoggerUtils.httpFailure,
  ~detailsOf=LoggerUtils.httpDetails,
  ~call,
) =>
  LoggerRuntime.observe(
    ~category=Api,
    ~spec=event->apiSpec,
    ~severity=apiSeverity,
    ~data=event->LoggerUtils.eventDetails->Array.concat([("url", url->JSON.Encode.string)]),
    ~details,
    ~failureOf,
    ~detailsOf,
    ~paymentMethod,
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
  ~onLoad=() => (),
  ~onError=_ => (),
) =>
  LoggerRuntime.observeResource(
    ~spec=event->resourceSpec,
    ~severity=resourceSeverity,
    ~url,
    ~resource=event->resourceKind,
    ~attributes,
    ~matchQuery,
    ~dedupe,
    ~timeoutMs?,
    ~abandoned?,
    ~paymentMethod,
    ~onLoad,
    ~onError,
  )
