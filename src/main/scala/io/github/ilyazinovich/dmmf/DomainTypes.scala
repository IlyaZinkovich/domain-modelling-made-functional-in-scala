package io.github.ilyazinovich.dmmf

case class Error(cause: String)

case class String50 private(string: String)

case class EmailAddress private(string: String)

case class ZipCode private(string: String)

case class OrderId private(string: String)

case class OrderLineId private(string: String)

sealed trait ProductCode

case class WidgetCode private(string: String) extends ProductCode

case class GadgetCode private(string: String) extends ProductCode

sealed trait ProductQuantity

case class UnitQuantity private(integer: Int) extends ProductQuantity

case class KilogramQuantity private(double: Double) extends ProductQuantity

case class Price private(double: Double)

case class BillingAmount private(double: Double)

case class PdfAttachment(name: String, bytes: Array[Byte])

object String50 {

  def create(string: String): Either[Error, String50] = {
    if (string.length <= 50) Right(new String50(string))
    else Left(Error("String is longer than 50 characters"))
  }

  private def apply(string: String) = new String50(string)
}

object EmailAddress {

  private val EmailPattern = "(.*@.*)".r

  def create(string: String): Either[Error, EmailAddress] = {
    string match {
      case "" => Left(Error("Email cannot be empty"))
      case EmailPattern(email) => Right(new EmailAddress(email))
      case _ => Left(Error("String does not match email pattern"))
    }
  }

  private def apply(string: String): EmailAddress = new EmailAddress(string)
}

object ZipCode {

  private val ZipCodePattern = "(\\d{5})".r

  def create(string: String): Either[Error, ZipCode] = {
    string match {
      case ZipCodePattern(zipCodeString) => Right(new ZipCode(zipCodeString))
      case _ => Left(Error("Input is not 5-digit string"))
    }
  }

  private def apply(string: String): ZipCode = new ZipCode(string)
}

object OrderId {

  def create(string: String): Either[Error, OrderId] = {
    if (string.isEmpty) {
      Left(Error("OrderId cannot be empty"))
    } else if (string.length > 50) {
      Left(Error("OrderId cannot be longer than 50 characters"))
    } else {
      Right(new OrderId(string))
    }
  }

  private def apply(string: String): OrderId = new OrderId(string)
}

object OrderLineId {

  def create(string: String): Either[Error, OrderLineId] = {
    if (string.isEmpty) {
      Left(Error("OrderId cannot be empty"))
    } else if (string.length > 50) {
      Left(Error("OrderId cannot be longer than 50 characters"))
    } else {
      Right(new OrderLineId(string))
    }
  }

  private def apply(string: String): OrderLineId = new OrderLineId(string)
}

case class Address(addressLine: String)

case class OrderLine(orderLineId: OrderLineId, productCode: ProductCode, quantity: ProductQuantity)

case class Order(orderId: OrderId, customerInformation: CustomerInformation, orderLines: List[OrderLine])

object WidgetCode {

  private val WidgetCodePattern = "(W\\d{4})".r

  def create(string: String): Either[Error, WidgetCode] = {
    string match {
      case WidgetCodePattern(widgetCodeString) => Right(new WidgetCode(widgetCodeString))
      case _ => Left(Error("Input is not a 5 chars string starting with W"))
    }
  }

  private def apply(string: String): WidgetCode = new WidgetCode(string)
}

object GadgetCode {

  private val GadgetCodePattern = "(G\\d{4})".r

  def create(string: String): Either[Error, GadgetCode] = {
    string match {
      case GadgetCodePattern(gadgetCodeString) => Right(new GadgetCode(gadgetCodeString))
      case _ => Left(Error("Input is not a 5 chars string starting with G"))
    }
  }

  private def apply(string: String): GadgetCode = new GadgetCode(string)
}

object ProductCode {

  type CheckProductCodeExist = ProductCode => Boolean

  def create(string: String, checkProductCodeExist: CheckProductCodeExist): Either[Error, ProductCode] = {
    createWithoutVerifyingExistence(string)
      .filterOrElse(checkProductCodeExist, Error(s"Product code does not exist: $string"))
  }

  def stringValue(productCode: ProductCode): String = {
    productCode match {
      case WidgetCode(string) => string
      case GadgetCode(string) => string
    }
  }

  private def createWithoutVerifyingExistence(string: String): Either[Error, ProductCode] = {
    if (string.startsWith("W")) WidgetCode.create(string)
    else if (string.startsWith("G")) GadgetCode.create(string)
    else Left(Error(s"Unrecognized product code format: $string"))
  }
}

object UnitQuantity {

  def create(integer: Int): Either[Error, UnitQuantity] = {
    if (integer < 1) Left(Error("Input integer is less than 1"))
    else if (integer > 1000) Left(Error("Input integer is more than 1000"))
    else Right(new UnitQuantity(integer))
  }

  private def apply(integer: Int): UnitQuantity = new UnitQuantity(integer)
}

object KilogramQuantity {

  def create(double: Double): Either[Error, KilogramQuantity] = {
    if (double < 0.05D) Left(Error("Input double is less than 0.05"))
    else if (double > 100.00D) Left(Error("Input double is more than 100.00"))
    else Right(new KilogramQuantity(double))
  }

  private def apply(double: Double): KilogramQuantity = new KilogramQuantity(double)
}

object ProductQuantity {

  def decimalQuantity(orderQuantity: ProductQuantity): Double = {
    orderQuantity match {
      case UnitQuantity(integer) => integer.toDouble
      case KilogramQuantity(double) => double
    }
  }

  def create(productCode: ProductCode, quantity: Double): Either[Error, ProductQuantity] = {
    productCode match {
      case WidgetCode(_) => UnitQuantity.create(quantity.toInt)
      case GadgetCode(_) => KilogramQuantity.create(quantity)
      case _ => Left(Error(s"Unrecognised product code: $productCode"))
    }
  }
}

sealed trait PricingError {

  def message(): String
}

case class NegativePriceError(price: Double) extends PricingError {

  override def message(): String = s"Price $price cannot be less than 0.0"
}

case class TooHighPrice(price: Double, limit: Double) extends PricingError {

  override def message(): String = s"Price $price cannot be more than $limit"
}

case class TooLargeBillingAmount(billingAmount: Double, limit: Double) extends PricingError {

  override def message(): String = s"Billing amount $billingAmount cannot be more than $limit"
}

object Price {

  private val limit = 1000.00D

  def value(price: Price): Double = price match {
    case Price(value) => value
  }

  def multiply(price: Price, quantity: ProductQuantity): Either[PricingError, Price] = {
    val decimalQuantity = ProductQuantity.decimalQuantity(quantity)
    Price.create(decimalQuantity * price.double)
  }

  def create(decimalPrice: Double): Either[PricingError, Price] = {
    if (decimalPrice < 0.0D) Left(NegativePriceError(decimalPrice))
    else if (decimalPrice > limit) Left(TooHighPrice(decimalPrice, limit))
    else Right(new Price(decimalPrice))
  }

  private def apply(double: Double): Price = new Price(double)
}

object BillingAmount {

  private val limit = 10000.00D

  def total(prices: List[Price]): Either[PricingError, BillingAmount] = {
    val totalPrice = prices.map(Price.value).sum
    create(totalPrice)
  }

  def create(decimalBillingAmount: Double): Either[PricingError, BillingAmount] = {
    if (decimalBillingAmount > limit) Left(TooLargeBillingAmount(decimalBillingAmount, limit))
    else Right(new BillingAmount(decimalBillingAmount))
  }

  private def apply(double: Double): BillingAmount = new BillingAmount(double)
}

case class PricedOrderLine(orderLineId: OrderLineId, productCode: ProductCode,
                           productQuantity: ProductQuantity, price: Price)

case class PricedOrder(orderId: OrderId,
                       customerInformation: CustomerInformation,
                       pricedOrderLines: List[PricedOrderLine],
                       billingAmount: BillingAmount)

case class CustomerInformation(address: Address, emailAddress: EmailAddress)

case class UnvalidatedAddress(addressLine: String)

case class UnvalidatedOrderLine(orderLineId: String, productCode: String, quantity: Double)

case class UnvalidatedCustomerInformation(emailAddress: String, address: UnvalidatedAddress)

case class UnvalidatedOrder(orderId: String, customerInformation: UnvalidatedCustomerInformation,
                            orderLines: List[UnvalidatedOrderLine])

sealed trait AcknowledgementStatus

case object Sent extends AcknowledgementStatus

case object NotSent extends AcknowledgementStatus

case class AcknowledgementLetter(content: String)

case class Acknowledgement(emailAddress: EmailAddress, content: AcknowledgementLetter)

sealed trait PlaceOrderEvent

case class OrderPlaced(placedOrder: PricedOrder) extends PlaceOrderEvent

case class BillableOrderPlaced(orderId: OrderId, billingAddress: Address,
                               amountToBill: BillingAmount) extends PlaceOrderEvent

case class AcknowledgementSent(orderId: OrderId, emailAddress: EmailAddress) extends PlaceOrderEvent
