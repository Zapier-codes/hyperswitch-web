type cardType =
  | Credit
  | Debit
  | Dynamic(string)
  | Unspecified

type clickToPayProvider =
  | VisaUctp
  | VisaDirect
  | MastercardUctp
  | MastercardDirect

type walletType =
  | GooglePay
  | ApplePay
  | SamsungPay
  | Paypal
  | PaypalSdk
  | Paze
  | Venmo
  | Dynamic(string)
  | Unspecified

type payLaterType =
  | Klarna
  | Affirm
  | AfterpayClearpay
  | PayBright
  | Walley
  | Alma
  | Atome
  | Dynamic(string)
  | Unspecified

type openBankingType =
  | Plaid
  | Dynamic(string)
  | Unspecified

type simpleType =
  | Dynamic(string)
  | Unspecified

type paymentMethod =
  | Card(cardType)
  | Wallet(walletType)
  | PayLater(payLaterType)
  | OpenBanking(openBankingType)
  | BankRedirect(simpleType)
  | BankDebit(simpleType)
  | BankTransfer(simpleType)
  | CardRedirect(simpleType)
  | MobilePayment(simpleType)
  | Dynamic(string)

let name = value =>
  switch value {
  | Card(_) => "CARD"
  | Wallet(_) => "WALLET"
  | PayLater(_) => "PAY_LATER"
  | OpenBanking(_) => "OPEN_BANKING"
  | BankRedirect(_) => "BANK_REDIRECT"
  | BankDebit(_) => "BANK_DEBIT"
  | BankTransfer(_) => "BANK_TRANSFER"
  | CardRedirect(_) => "CARD_REDIRECT"
  | MobilePayment(_) => "MOBILE_PAYMENT"
  | Dynamic(value) => value->LoggerGrammar.screamingSnakeCase
  }

let typeName = value =>
  switch value {
  | Card(Credit) => Some("CREDIT")
  | Card(Debit) => Some("DEBIT")
  | Wallet(GooglePay) => Some("GOOGLE_PAY")
  | Wallet(ApplePay) => Some("APPLE_PAY")
  | Wallet(SamsungPay) => Some("SAMSUNG_PAY")
  | Wallet(Paypal) => Some("PAYPAL")
  | Wallet(PaypalSdk) => Some("PAYPAL_SDK")
  | Wallet(Paze) => Some("PAZE")
  | Wallet(Venmo) => Some("VENMO")
  | PayLater(Klarna) => Some("KLARNA")
  | PayLater(Affirm) => Some("AFFIRM")
  | PayLater(AfterpayClearpay) => Some("AFTERPAY_CLEARPAY")
  | PayLater(PayBright) => Some("PAY_BRIGHT")
  | PayLater(Walley) => Some("WALLEY")
  | PayLater(Alma) => Some("ALMA")
  | PayLater(Atome) => Some("ATOME")
  | OpenBanking(Plaid) => Some("PLAID")
  | Card(Dynamic(value))
  | Wallet(Dynamic(value))
  | PayLater(Dynamic(value))
  | OpenBanking(Dynamic(value))
  | BankRedirect(Dynamic(value))
  | BankDebit(Dynamic(value))
  | BankTransfer(Dynamic(value))
  | CardRedirect(Dynamic(value))
  | MobilePayment(Dynamic(value)) =>
    Some(value->LoggerGrammar.screamingSnakeCase)
  | Card(Unspecified)
  | Wallet(Unspecified)
  | PayLater(Unspecified)
  | OpenBanking(Unspecified)
  | BankRedirect(Unspecified)
  | BankDebit(Unspecified)
  | BankTransfer(Unspecified)
  | CardRedirect(Unspecified)
  | MobilePayment(Unspecified)
  | Dynamic(_) =>
    None
  }

let cardSubtype = value =>
  switch value {
  | "credit" => Credit
  | "debit" => Debit
  | other => Dynamic(other)
  }

let walletSubtype = value =>
  switch value {
  | "google_pay" => GooglePay
  | "apple_pay" => ApplePay
  | "samsung_pay" => SamsungPay
  | "paypal" => Paypal
  | "paypal_sdk" => PaypalSdk
  | "paze" => Paze
  | "venmo" => Venmo
  | other => Dynamic(other)
  }

let payLaterSubtype = value =>
  switch value {
  | "klarna" => Klarna
  | "affirm" => Affirm
  | "afterpay_clearpay" => AfterpayClearpay
  | "pay_bright" => PayBright
  | "walley" => Walley
  | "alma" => Alma
  | "atome" => Atome
  | other => Dynamic(other)
  }

let openBankingSubtype = value =>
  switch value {
  | "plaid" => Plaid
  | other => Dynamic(other)
  }

let fromBackendValue = value =>
  switch value->String.toLowerCase->String.trim {
  | "" => None
  | ("credit" | "debit") as subtype => Some(Card(subtype->cardSubtype))
  | ("google_pay"
    | "apple_pay"
    | "samsung_pay"
    | "paypal"
    | "paypal_sdk"
    | "paze"
    | "venmo") as subtype =>
    Some(Wallet(subtype->walletSubtype))
  | ("klarna"
    | "affirm"
    | "afterpay_clearpay"
    | "pay_bright"
    | "walley"
    | "alma"
    | "atome") as subtype =>
    Some(PayLater(subtype->payLaterSubtype))
  | "plaid" as subtype => Some(OpenBanking(subtype->openBankingSubtype))
  | "card" => Some(Card(Unspecified))
  | "wallet" => Some(Wallet(Unspecified))
  | "pay_later" => Some(PayLater(Unspecified))
  | "open_banking" => Some(OpenBanking(Unspecified))
  | "bank_redirect" => Some(BankRedirect(Unspecified))
  | "bank_debit" => Some(BankDebit(Unspecified))
  | "bank_transfer" => Some(BankTransfer(Unspecified))
  | "card_redirect" => Some(CardRedirect(Unspecified))
  | "mobile_payment" => Some(MobilePayment(Unspecified))
  | other => Some(Dynamic(other))
  }

let withType = (value, typeValue) =>
  switch (value, typeValue->String.toLowerCase->String.trim) {
  | (value, "") => value
  | (Card(Unspecified), subtype) => Card(subtype->cardSubtype)
  | (Wallet(Unspecified), subtype) => Wallet(subtype->walletSubtype)
  | (PayLater(Unspecified), subtype) => PayLater(subtype->payLaterSubtype)
  | (OpenBanking(Unspecified), subtype) => OpenBanking(subtype->openBankingSubtype)
  | (BankRedirect(Unspecified), subtype) => BankRedirect(Dynamic(subtype))
  | (BankDebit(Unspecified), subtype) => BankDebit(Dynamic(subtype))
  | (BankTransfer(Unspecified), subtype) => BankTransfer(Dynamic(subtype))
  | (CardRedirect(Unspecified), subtype) => CardRedirect(Dynamic(subtype))
  | (MobilePayment(Unspecified), subtype) => MobilePayment(Dynamic(subtype))
  | (value, _) => value
  }

let fromBackendPair = (~method, ~methodType) =>
  switch method->fromBackendValue {
  | Some(value) => Some(value->withType(methodType))
  | None => methodType->fromBackendValue
  }

let qualifiedName = value =>
  switch value->typeName {
  | Some(typeName) => `${value->name}.${typeName}`
  | None => value->name
  }

let refine = (existing: option<paymentMethod>, incoming: paymentMethod) =>
  switch existing {
  | Some(existing) if existing->name === incoming->name =>
    switch (existing->typeName, incoming->typeName) {
    | (Some(_), None) => existing
    | _ => incoming
    }
  | _ => incoming
  }
