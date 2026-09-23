type cardType = Unspecified

type walletType =
  | GooglePay
  | ApplePay
  | SamsungPay
  | Paypal
  | PaypalSdk
  | Paze

type payLaterType = Klarna

type openBankingType =
  | Plaid
  | Unspecified

type paymentMethod =
  | Card(cardType)
  | Wallet(walletType)
  | PayLater(payLaterType)
  | OpenBanking(openBankingType)
  | Dynamic(string)

let fromBackendPair = (~method, ~methodType) =>
  switch [method, methodType]->Array.map(String.trim)->Array.filter(part => part !== "") {
  | [] => None
  | parts => Some(Dynamic(parts->Array.join(".")))
  }

let fromBackendValue = value => fromBackendPair(~method=value, ~methodType="")

let qualifiedName = value => {
  let family = value->LoggerUtils.variantName
  let qualify = subtype => `${family}.${subtype->LoggerUtils.variantName}`
  switch value {
  | Dynamic(raw) => raw->LoggerUtils.snakeCase
  | Card(Unspecified) | OpenBanking(Unspecified) => family
  | Wallet(subtype) => subtype->qualify
  | PayLater(subtype) => subtype->qualify
  | OpenBanking(subtype) => subtype->qualify
  }
}
