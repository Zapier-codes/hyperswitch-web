open LoggerTypes

type walletStage =
  | ConfirmRequestReceived
  | OneClickDeclined

type walletFlow =
  | Normal
  | Delayed
  | ThirdParty
  | PaypalSdkTabs

type walletFailure =
  | MissingIntent
  | MissingCurrency
  | MissingNonce
  | ConnectorUnsupported
  | ClientUnavailable
  | PaymentsNotAllowed
  | ClientCreationFailed
  | AvailabilityCheckFailed
  | ListenerSetupFailed
  | MessageHandlingFailed
  | PaymentDataFailed
  | SdkMountFailed

type vaultFailure =
  | FieldBindingFailed
  | FieldMountFailed
  | FieldUpdateFailed
  | FieldUnmountFailed
  | FormCreationFailed

type threeDsMethodFailure =
  | MissingContainer
  | FormSubmitFailed
  | IframeLoadFailed

type threeDsPopupFailure = MessageHandlingFailed

type ddcFailure =
  | MissingUrl
  | MissingRedirectUrl
  | InvalidNextAction
  | UnreadableResponse

type walletStageData = {stage: walletStage, connector?: string}
type walletFlowData = {flow: walletFlow, connector?: string}

type walletFailureData = {reason: walletFailure, connector?: string}
type vaultFailureData = {reason: vaultFailure}
type transStatusData = {transStatus: string}

type threeDsMethodFailureData = {reason: threeDsMethodFailure}
type threeDsPopupFailureData = {reason: threeDsPopupFailure}
type ddcFailureData = {reason: ddcFailure}

type paymentOutcomeData = {status: string, manualRetryAllowed?: bool}
type customerRedirectData = {nextAction: string, redirectMode?: string, redirectOrigin: string}
type redirectFailureData = {nextAction: string, recovered: bool}
type unknownPaymentMethodData = {value: string}
type unsupportedConnectorData = {connector: string}
type paymentStatusUnknownData = {inferred: bool}

type lifecycleEvent =
  | ElementIframeMounted
  | AppRendered
  | PaymentSucceeded(paymentOutcomeData)
  | PaymentFailed(paymentOutcomeData)
  | PaymentRejected
  | PaymentStatusUnknown(paymentStatusUnknownData)
  | WalletFlowResolved(walletFlowData)
  | WalletStageReached(walletStageData)
  | WalletFlowFailed(walletFailureData)
  | WalletFlowExited
  | WalletTokenReceived
  | VaultFlowFailed(vaultFailureData)
  | BankAuthSyncFailed({status: string})
  | BankAuthConnectorUnsupported(unsupportedConnectorData)
  | CustomerRedirectStarted(customerRedirectData)
  | RedirectUnsupported(redirectFailureData)
  | ThreeDsPopupRequested
  | ThreeDsPopupFailed(threeDsPopupFailureData)
  | ThreeDsChallengeShown(transStatusData)
  | ThreeDsFrictionlessResolved(transStatusData)
  | ThreeDsAuthContainerMissing(transStatusData)
  | ThreeDsAuthRequestFailed
  | ThreeDsMethodStarted
  | ThreeDsMethodCompleted
  | ThreeDsMethodFailed(threeDsMethodFailureData)
  | ThreeDsMethodTimedOut
  | DdcStarted
  | DdcCompleted
  | DdcFailed(ddcFailureData)
  | DdcTimedOut
  | QrCodeShown
  | VoucherShown
  | BankTransferShown
  | PaymentMethodUnresolved(unknownPaymentMethodData)
  | CountryDataServedFromBundle
  | CountryDataUnavailable
  | EligibilityCheckCancelled
  | EligibilityCheckFailed

let walletFailureSeverity = reason =>
  switch reason {
  | MissingIntent
  | MissingCurrency
  | ConnectorUnsupported
  | PaymentsNotAllowed
  | ClientCreationFailed
  | AvailabilityCheckFailed
  | ListenerSetupFailed
  | SdkMountFailed =>
    Warning
  | MissingNonce
  | ClientUnavailable
  | MessageHandlingFailed
  | PaymentDataFailed =>
    Error
  }

let threeDsMethodFailureSeverity = reason =>
  switch reason {
  | MissingContainer | FormSubmitFailed => Error
  | IframeLoadFailed => Warning
  }

let vaultFailureSeverity = reason =>
  switch reason {
  | FieldBindingFailed
  | FieldMountFailed
  | FormCreationFailed =>
    Error
  | FieldUpdateFailed
  | FieldUnmountFailed =>
    Warning
  }

let lifecycleSeverity = value =>
  switch value {
  | ElementIframeMounted
  | WalletFlowResolved(_)
  | WalletStageReached(_)
  | ThreeDsPopupRequested
  | ThreeDsMethodStarted
  | ThreeDsMethodCompleted
  | DdcStarted
  | DdcCompleted
  | CountryDataServedFromBundle
  | EligibilityCheckCancelled =>
    Debug
  | AppRendered
  | PaymentSucceeded(_)
  | PaymentFailed(_)
  | WalletTokenReceived
  | CustomerRedirectStarted(_)
  | ThreeDsChallengeShown(_)
  | ThreeDsFrictionlessResolved(_)
  | QrCodeShown
  | VoucherShown
  | BankTransferShown =>
    Info
  | ThreeDsMethodTimedOut
  | DdcTimedOut
  | BankAuthConnectorUnsupported(_)
  | PaymentMethodUnresolved(_)
  | PaymentStatusUnknown(_)
  | WalletFlowExited
  | EligibilityCheckFailed =>
    Warning
  | PaymentRejected
  | ThreeDsAuthContainerMissing(_)
  | ThreeDsAuthRequestFailed
  | BankAuthSyncFailed(_)
  | CountryDataUnavailable
  | DdcFailed(_) =>
    Error
  | ThreeDsPopupFailed(_) => Error
  | RedirectUnsupported({recovered}) => recovered ? Warning : Error
  | VaultFlowFailed({reason}) => reason->vaultFailureSeverity
  | WalletFlowFailed({reason}) => reason->walletFailureSeverity
  | ThreeDsMethodFailed({reason}) => reason->threeDsMethodFailureSeverity
  }

type cardFormScope = PaymentForm | VaultForm
type loaderState = Loading | SemiLoaded | Loaded | LoadFailed

type networkData = {online: bool}
type cardFormData = {scope: cardFormScope}
type cardFieldData = {scope: cardFormScope, field: string}
type loaderData = {state: loaderState}
type clickToPayViewData = {view: string}
type updateIntentData = {inProgress: bool}
type formCompletionData = {savedMethod: bool}

type stateEvent =
  | NetworkStatusChanged(networkData)
  | ElementOptionsChanged
  | LoaderStateChanged(loaderData)
  | CardFormMounted(cardFormData)
  | CardFormUnmounted(cardFormData)
  | CardFieldMounted(cardFieldData)
  | CardFieldUnmounted(cardFieldData)
  | DynamicFieldsChanged
  | PaymentFormCompleted(formCompletionData)
  | UpdateIntentProgressChanged(updateIntentData)
  | ClickToPayViewChanged(clickToPayViewData)

let stateSeverity = value =>
  switch value {
  | LoaderStateChanged({state: LoadFailed}) => Error
  | LoaderStateChanged({state: Loaded}) => Info
  | NetworkStatusChanged({online}) => online ? Debug : Warning
  | LoaderStateChanged(_)
  | ElementOptionsChanged
  | CardFormMounted(_)
  | CardFormUnmounted(_)
  | CardFieldMounted(_)
  | CardFieldUnmounted(_)
  | DynamicFieldsChanged
  | PaymentFormCompleted(_)
  | UpdateIntentProgressChanged(_)
  | ClickToPayViewChanged(_) =>
    Debug
  }

type view =
  | SavedMethodList
  | MorePaymentMethods
  | InstallmentOptions

type openedView =
  | NewPaymentMethods
  | ManageSavedMethod
  | ClickToPayIdentityChange
  | CardSchemeMenu

type submitSource =
  | PayButton
  | SaveCardButton

type verificationSource =
  | ClickToPayOtp
  | ClickToPayIdentity

type fieldData = {field: string}
type fieldToggleData = {field: string, enabled: bool}
type methodData = {method: string}
type savedMethodSelectionData = {requiresCvv: bool, isCardExpired: bool}
type viewData = {view: view, expanded: bool}
type openedViewData = {view: openedView}
type submitData = {source: submitSource}

type verificationData = {
  source: verificationSource,
  provider: option<LoggerTaxonomy.clickToPayProvider>,
}

type userEvent =
  | PaymentMethodSelected(methodData)
  | CardSchemeSelected(methodData)
  | SavedMethodSelected(savedMethodSelectionData)
  | SavedMethodUpdateRequested
  | SavedMethodDeleteRequested
  | PaymentSubmitted(submitData)
  | CustomerVerificationSubmitted(verificationData)
  | BankDetailsConfirmed
  | ExpressCheckoutClicked
  | ExpressCheckoutDismissed
  | VoucherDownloadRequested
  | QrCodeCopyRequested
  | ThreeDsPopupDismissed
  | ClickToPayOtpResendRequested
  | FieldEdited(fieldData)
  | FieldToggled(fieldToggleData)
  | FieldFocused(fieldData)
  | FieldBlurred(fieldData)
  | ViewToggled(viewData)
  | ViewOpened(openedViewData)

let userSeverity = value =>
  switch value {
  | PaymentMethodSelected(_)
  | CardSchemeSelected(_)
  | SavedMethodSelected(_)
  | SavedMethodUpdateRequested
  | SavedMethodDeleteRequested
  | PaymentSubmitted(_)
  | CustomerVerificationSubmitted(_)
  | ExpressCheckoutClicked
  | ExpressCheckoutDismissed
  | ThreeDsPopupDismissed
  | VoucherDownloadRequested
  | QrCodeCopyRequested
  | BankDetailsConfirmed
  | ClickToPayOtpResendRequested =>
    Info
  | FieldEdited(_)
  | FieldToggled(_)
  | FieldFocused(_)
  | FieldBlurred(_)
  | ViewToggled(_)
  | ViewOpened(_) =>
    Debug
  }

type vaultTokenizeScope =
  | SaveCardCvc
  | FullCard
type vaultTokenizeData = {scope: vaultTokenizeScope}

type apiEvent =
  | RetrievePaymentIntent
  | ConfirmCall
  | ConfirmPayoutCall
  | CompleteAuthorize
  | PostSessionTokens
  | Sessions
  | Authentication
  | PollStatus
  | PaymentMethodsList
  | CreateCustomerPaymentMethods
  | RetrievePaymentMethodSession
  | SavePaymentMethod
  | UpdatePaymentMethod
  | DeletePaymentMethod
  | PaymentMethodEligibility
  | PaymentMethodsAuthLink
  | PaymentMethodsAuthExchange
  | TaxCalculation
  | ClientList
  | VaultTokenization(vaultTokenizeData)

let apiSpec = value => makeOperation(request, value->LoggerUtils.variantName)

let apiSeverity = value =>
  switch value {
  | Sessions
  | TaxCalculation
  | PaymentMethodEligibility
  | PollStatus => {success: Info, failure: Warning}
  | _ => {success: Info, failure: Error}
  }

type functionEvent =
  | LoadPaymentSheet
  | LoadPaymentData
  | IsReadyToPay
  | FinishApplePaymentV2
  | ExecuteGooglePayment
  | BraintreeClientCreate
  | BraintreeApplePayCreate
  | BraintreePerformValidation
  | BraintreeTokenize
  | KlarnaInit
  | KlarnaLoad
  | PaypalButtonsRender
  | PlaidCreate
  | VaultFormCreate

let functionSpec = value => makeOperation(call, value->LoggerUtils.variantName)

let functionSeverity = value =>
  switch value {
  | IsReadyToPay => {success: Debug, failure: Warning}
  | LoadPaymentSheet
  | LoadPaymentData
  | FinishApplePaymentV2
  | ExecuteGooglePayment
  | BraintreeClientCreate
  | BraintreeApplePayCreate
  | BraintreePerformValidation
  | BraintreeTokenize
  | KlarnaInit
  | KlarnaLoad
  | PaypalButtonsRender
  | PlaidCreate
  | VaultFormCreate => {success: Debug, failure: Error}
  }

type resourceEvent =
  | GooglePayScript
  | SamsungPayScript
  | ApplePayScript
  | PaypalScript
  | PazeScript
  | KlarnaScript
  | TrustpayScript
  | BraintreeClientScript
  | BraintreeApplePayScript
  | PmAuthConnectorScript
  | VaultScript
  | FontStylesheet

let resourceSpec = value => makeOperation(load, value->LoggerUtils.variantName)

let resourceSeverity = value =>
  switch value {
  | VaultScript => {success: Debug, failure: Error}
  | GooglePayScript
  | SamsungPayScript
  | ApplePayScript
  | PaypalScript
  | PazeScript
  | KlarnaScript
  | TrustpayScript
  | BraintreeClientScript
  | BraintreeApplePayScript
  | PmAuthConnectorScript
  | FontStylesheet => {success: Debug, failure: Warning}
  }

type staticAssetEvent =
  | CountryStateData
  | SdkConfigs

let staticAssetSpec = value => makeOperation(load, value->LoggerUtils.variantName)

let staticAssetSeverity = value =>
  switch value {
  | SdkConfigs => {success: Debug, failure: Error}
  | CountryStateData => {success: Debug, failure: Warning}
  }

let resourceKind = (value): ResourceLoader.resource =>
  switch value {
  | FontStylesheet => Stylesheet
  | _ => Script
  }

type crashOrigin =
  | ErrorBoundary
  | UncaughtError
  | UnhandledRejection
  | EntryPoint
  | ElementConstructor
  | ParentWindowMessage

type degradedSurface =
  | WalletButton
  | PaymentMethodPane

let degradedSeverity = surface =>
  switch surface {
  | WalletButton => Warning
  | PaymentMethodPane => Error
  }

let logLifecycle = (
  ~event: lifecycleEvent,
  ~details=[],
  ~exn=?,
  ~failure=?,
  ~durationMs=?,
  ~paymentMethod=?,
) =>
  LoggerRuntime.emit(
    ~category=Lifecycle,
    ~spec=event->LoggerUtils.deriveEvent,
    ~severity=event->lifecycleSeverity,
    ~data=event->LoggerUtils.eventDetails,
    ~details,
    ~exn?,
    ~failure?,
    ~durationMs?,
    ~paymentMethod?,
  )

let logState = (
  ~event: stateEvent,
  ~details=[],
  ~exn=?,
  ~failure=?,
  ~durationMs=?,
  ~paymentMethod=?,
) =>
  LoggerRuntime.emit(
    ~category=State,
    ~spec=event->LoggerUtils.deriveNotification,
    ~severity=event->stateSeverity,
    ~data=event->LoggerUtils.eventDetails,
    ~details,
    ~exn?,
    ~failure?,
    ~durationMs?,
    ~paymentMethod?,
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

let editedFields = ref(("", Set.make()))

let isFirstEditOf = field => {
  let sessionId = LoggerContext.current().sessionId
  let (knownSessionId, fields) = editedFields.contents
  let fields = if knownSessionId === sessionId {
    fields
  } else {
    let fields = Set.make()
    editedFields := (sessionId, fields)
    fields
  }
  fields->Set.has(field)
    ? false
    : {
        fields->Set.add(field)
        true
      }
}

let logUser = (~event: userEvent, ~details=[], ~paymentMethod=?) => {
  let event = event->identify
  switch (event, paymentMethod) {
  | (PaymentMethodSelected({method}), None) =>
    method->LoggerTaxonomy.fromBackendValue->Option.forEach(LoggerContext.setPaymentMethod)
  | (PaymentMethodSelected(_), Some(paymentMethod)) => LoggerContext.setPaymentMethod(paymentMethod)
  | _ => ()
  }
  let shouldEmit = switch event {
  | FieldEdited({field}) => field->isFirstEditOf
  | _ => true
  }
  if shouldEmit {
    LoggerRuntime.emit(
      ~category=User,
      ~spec=event->LoggerUtils.deriveNotification,
      ~severity=event->userSeverity,
      ~data=event->LoggerUtils.eventDetails,
      ~details,
      ~paymentMethod?,
    )
  }
}

let logCrash = (~origin: crashOrigin, ~exn=?, ~details=[]) =>
  LoggerRuntime.emit(
    ~category=Crash,
    ~spec=origin->LoggerUtils.deriveEvent,
    ~severity=Error,
    ~details,
    ~exn?,
  )

let logDegraded = (~surface: degradedSurface, ~exn=?, ~details=[]) =>
  LoggerRuntime.emit(
    ~category=Lifecycle,
    ~spec=surface->LoggerUtils.deriveFailure,
    ~severity=surface->degradedSeverity,
    ~details,
    ~exn?,
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

  Window.addEventListener("error", (event: JSON.t) => {
    let source = event->text("filename")
    if source->isOurs {
      report(
        ~origin=UncaughtError,
        ~details=[
          ("error_message", event->field("message")->describe->JSON.Encode.string),
          ("error_source", source->JSON.Encode.string),
        ],
      )
    }
  })

  Window.addEventListener("unhandledrejection", (event: JSON.t) => {
    let reason = event->field("reason")
    let stack = reason->text("stack")
    if stack->isOurs {
      report(
        ~origin=UnhandledRejection,
        ~details=[
          ("error_message", reason->describe->JSON.Encode.string),
          ("error_source", stack->JSON.Encode.string),
        ],
      )
    }
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
  ~call,
) =>
  LoggerRuntime.observe(
    ~category=Api,
    ~spec=event->apiSpec,
    ~severity=event->apiSeverity,
    ~data=event->LoggerUtils.eventDetails->Array.concat([("url", url->JSON.Encode.string)]),
    ~details,
    ~timeoutMs?,
    ~failureOf,
    ~detailsOf,
    ~paymentMethod?,
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
  ~call,
) =>
  LoggerRuntime.observe(
    ~category=Resource,
    ~spec=event->staticAssetSpec,
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
    ~call,
  )

let logApi = (~event: apiEvent, ~outcome, ~details=[], ~startedAt=?, ~exn=?, ~paymentMethod=?) =>
  LoggerRuntime.emitPhase(
    ~category=Api,
    ~spec=event->apiSpec,
    ~severity=event->apiSeverity,
    ~outcome,
    ~data=event->LoggerUtils.eventDetails,
    ~details,
    ~startedAt?,
    ~exn?,
    ~paymentMethod?,
  )

let logFunction = (
  ~event: functionEvent,
  ~outcome,
  ~details=[],
  ~startedAt=?,
  ~exn=?,
  ~paymentMethod=?,
) =>
  LoggerRuntime.emitPhase(
    ~category=Function,
    ~spec=event->functionSpec,
    ~severity=event->functionSeverity,
    ~outcome,
    ~data=event->LoggerUtils.eventDetails,
    ~details,
    ~startedAt?,
    ~exn?,
    ~paymentMethod?,
  )

let observeFunction = (
  ~event: functionEvent,
  ~details=[],
  ~timeoutMs=?,
  ~failureOf=?,
  ~detailsOf=?,
  ~paymentMethod=?,
  ~call,
) =>
  LoggerRuntime.observe(
    ~category=Function,
    ~spec=event->functionSpec,
    ~severity=event->functionSeverity,
    ~data=event->LoggerUtils.eventDetails,
    ~details,
    ~timeoutMs?,
    ~failureOf?,
    ~detailsOf?,
    ~paymentMethod?,
    ~call,
  )

let observeResource = (
  ~event: resourceEvent,
  ~url,
  ~attributes=[],
  ~matchQuery=false,
  ~dedupe=true,
  ~paymentMethod=?,
  ~abandoned=() => false,
  ~onLoad=() => (),
  ~onError=_ => (),
) =>
  LoggerRuntime.observeResource(
    ~spec=event->resourceSpec,
    ~severity=event->resourceSeverity,
    ~url,
    ~resource=event->resourceKind,
    ~attributes,
    ~matchQuery,
    ~dedupe,
    ~paymentMethod?,
    ~abandoned,
    ~onLoad,
    ~onError,
  )
