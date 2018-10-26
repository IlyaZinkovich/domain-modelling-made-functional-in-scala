package io.github.ilyazinovich.dmmf

case class Error(cause: String)

case class String50 private(value: String)

case class EmailAddress private(value: String)

case class ZipCode private(value: String)

case class OrderId private(value: String)

case class OrderLineId private(value: String)

sealed trait ProductCode

case class WidgetCode private(value: String) extends ProductCode

case class GadgetCode private(value: String) extends ProductCode

sealed trait ProductQuantity

case class UnitQuantity private(value: Int) extends ProductQuantity

case class KilogramQuantity private(value: Double) extends ProductQuantity

case class Price private(value: Double)

case class BillingAmount private(value: Double)

case class PdfAttachment(name: String, bytes: Array[Byte])

object String50 {

  def create(value: String): Either[Error, String50] = {
    if (value.length <= 50) Right(new String50(value))
    else Left(Error("String is longer than 50 characters"))
  }

  private def apply(value: String) = new String50(value)
}

object EmailAddress {

  private val EmailPattern = "(.*@.*)".r

  def create(value: String): Either[Error, EmailAddress] = {
    value match {
      case "" => Left(Error("Email cannot be empty"))
      case EmailPattern(email) => Right(new EmailAddress(email))
      case _ => Left(Error("String does not match email pattern"))
    }
  }

  private def apply(value: String): EmailAddress = new EmailAddress(value)
}

object ZipCode {

  private val ZipCodePattern = "(\\d{5})".r

  def create(value: String): Either[Error, ZipCode] = {
    value match {
      case ZipCodePattern(zipCodeString) => Right(new ZipCode(zipCodeString))
      case _ => Left(Error("Input is not 5-digit string"))
    }
  }

  private def apply(value: String): ZipCode = new ZipCode(value)
}

object OrderId {

  def create(value: String): Either[Error, OrderId] = {
    if (value.isEmpty) Left(Error("OrderId cannot be empty"))
    else if (value.length > 50) Left(Error("OrderId cannot be longer than 50 characters"))
    else Right(new OrderId(value))
  }

  private def apply(value: String): OrderId = new OrderId(value)
}

object OrderLineId {

  def create(value: String): Either[Error, OrderLineId] = {
    if (value.isEmpty) Left(Error("OrderId cannot be empty"))
    else if (value.length > 50) Left(Error("OrderId cannot be longer than 50 characters"))
    else Right(new OrderLineId(value))
  }

  private def apply(value: String): OrderLineId = new OrderLineId(value)
}

case class Address(addressLine: String)

case class OrderLine(orderLineId: OrderLineId, productCode: ProductCode, quantity: ProductQuantity)

case class Order(orderId: OrderId, customerInformation: CustomerInformation, orderLines: List[OrderLine])

object WidgetCode {

  private val WidgetCodePattern = "(W\\d{4})".r

  def create(value: String): Either[Error, WidgetCode] = {
    value match {
      case WidgetCodePattern(widgetCodeString) => Right(new WidgetCode(widgetCodeString))
      case _ => Left(Error("Input is not a 5 chars string starting with W"))
    }
  }

  private def apply(value: String): WidgetCode = new WidgetCode(value)
}

object GadgetCode {

  private val GadgetCodePattern = "(G\\d{4})".r

  def create(value: String): Either[Error, GadgetCode] = {
    value match {
      case GadgetCodePattern(gadgetCodeString) => Right(new GadgetCode(gadgetCodeString))
      case _ => Left(Error("Input is not a 5 chars string starting with G"))
    }
  }

  private def apply(value: String): GadgetCode = new GadgetCode(value)
}

object ProductCode {

  type CheckProductCodeExist = ProductCode => Boolean

  def create(value: String, checkProductCodeExist: CheckProductCodeExist): Either[Error, ProductCode] = {
    createWithoutVerifyingExistence(value)
      .filterOrElse(checkProductCodeExist, Error(s"Product code does not exist: $value"))
  }

  def value(productCode: ProductCode): String = {
    productCode match {
      case WidgetCode(value) => value
      case GadgetCode(value) => value
    }
  }

  private def createWithoutVerifyingExistence(value: String): Either[Error, ProductCode] = {
    if (value.startsWith("W")) WidgetCode.create(value)
    else if (value.startsWith("G")) GadgetCode.create(value)
    else Left(Error(s"Unrecognized product code format: $value"))
  }
}

object UnitQuantity {

  def create(value: Int): Either[Error, UnitQuantity] = {
    if (value < 1) Left(Error("Input integer is less than 1"))
    else if (value > 1000) Left(Error("Input integer is more than 1000"))
    else Right(new UnitQuantity(value))
  }

  private def apply(value: Int): UnitQuantity = new UnitQuantity(value)
}

object KilogramQuantity {

  def create(value: Double): Either[Error, KilogramQuantity] = {
    if (value < 0.05D) Left(Error("Input double is less than 0.05"))
    else if (value > 100.00D) Left(Error("Input double is more than 100.00"))
    else Right(new KilogramQuantity(value))
  }

  private def apply(value: Double): KilogramQuantity = new KilogramQuantity(value)
}

object ProductQuantity {

  def decimalQuantity(orderQuantity: ProductQuantity): Double = {
    orderQuantity match {
      case UnitQuantity(value) => value.toDouble
      case KilogramQuantity(value) => value
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
    Price.create(decimalQuantity * price.value)
  }

  def create(decimalPrice: Double): Either[PricingError, Price] = {
    if (decimalPrice < 0.0D) Left(NegativePriceError(decimalPrice))
    else if (decimalPrice > limit) Left(TooHighPrice(decimalPrice, limit))
    else Right(new Price(decimalPrice))
  }

  private def apply(value: Double): Price = new Price(value)
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

  private def apply(value: Double): BillingAmount = new BillingAmount(value)
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
