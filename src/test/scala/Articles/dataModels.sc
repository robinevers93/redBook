import java.time.ZonedDateTime
import java.util.UUID
import java.util.concurrent.RejectedExecutionHandler

case class Invoice(
                    companyName: String,
                    taxId: String,
                    street: String,
                    zipCode: String,
                    city: String,
                    country: String,
                    amount: BigDecimal,
                    currency: String,
                    payableUntil: ZonedDateTime
                  )
Invoice(
  companyName = "123-45-67-890",
  taxId = "",
  street = "SW1A 0AA",
  zipCode = "",
  city = "UK",
  country = "London",
  amount = BigDecimal(10),
  currency = "Oxford Street",
  payableUntil = ZonedDateTime.now().plusWeeks(2)
)

//entities are objects that have a domain meaning with an identity and often a life cycle -> order/customer/shipment/contract

//value objects are values with a domain meaning. They lack identity and lifecycle -> money, address, company name

//more meaningful types -> can't confuse fields
case class InvoiceId(value: UUID)
case class CompanyName(value: String)
case class TaxId(value: String)
case class Street(value: String)
case class ZipCode(value: String)
case class City(value: String)
case class Country(value: String)
case class Amount(value: BigDecimal)
case class Currency(value: String)

case class Invoice(
                    invoiceId: InvoiceId,
                    companyName: CompanyName,
                    taxId: TaxId,
                    street: Street,
                    zipCode: ZipCode,
                    city: City,
                    country: Country,
                    amount: Amount,
                    currency: Currency,
                    payableUntil: ZonedDateTime
                  )

//extract meaningful values to separate classes -> value objects
//value objects are now part of an invoice case class -> entity
case class BillingAddress(
                           street: Street,
                           zipCode: ZipCode,
                           city: City,
                           country: Country
                         )
case class InvoiceId(value: UUID)
case class Company(
                    name: CompanyName,
                    billingAddress: BillingAddress,
                    taxId: TaxId
                  )

sealed trait CurrencyTrait{
  def exchangeToEUR: Double
}

case object PLN extends CurrencyTrait{
  override def exchangeToEUR: Double = 3
}
case object EUR extends CurrencyTrait{
  override def exchangeToEUR: Double = 1
}
case object GBP extends CurrencyTrait{
  override def exchangeToEUR: Double = 2
}

trait ExchangeRate[A <: CurrencyTrait,B <: CurrencyTrait]{
  def rate(a:A,b:B): Double
}

object ExchangeRate{
//  implicit val pln2eur: ExchangeRate[PLN, EUR] = new ExchangeRate[PLN, EUR] {
//    override def rate(a: PLN, b: EUR):Double = a.exhangeToEUR/b.exhangeToEUR
//  }
//  implicit val gbp2eur: ExchangeRate[GBP, EUR] = new ExchangeRate[GBP, EUR] {
//    override def rate(a: GBP, b: EUR):Double = a.exhangeToEUR/b.exhangeToEUR
//  }
  implicit def exchange[A <: CurrencyTrait,B <: CurrencyTrait]: ExchangeRate[A, B] = new ExchangeRate[A,B] {
    override def rate(a: A, b: B) = a.exchangeToEUR/b.exchangeToEUR
  }
}


case class Money(amount: BigDecimal, currency: CurrencyTrait){

  def +(other: Money) = Money(this.amount + other.amount, this.currency)
  def change_currency(otherCurrency: CurrencyTrait)(implicit exchangeRate: ExchangeRate[this.currency.type, otherCurrency.type]) = {
    Money(this.amount*exchangeRate.rate(this.currency,otherCurrency), otherCurrency)
  }
}

def +(x:Money, y:Money): Money = if (x.currency == y.currency) Money(x.amount+y.amount, x.currency) else x

case class Invoice(
                    invoiceId: InvoiceId,
                    company: Company,
                    amount: Money,
                    payableUntil: ZonedDateTime
                  )

val money1 = Money(BigDecimal(5), EUR)
val money2 = Money(BigDecimal(3), EUR)

money1 + money2

money1.change_currency(GBP)


"hello" + " Victor"

