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

  private def apply(string: String) = new String50(string)

  def create(string: String): Either[Error, String50] = {
    if (string.length <= 50) Right(new String50(string))
    else Left(Error("String is longer than 50 characters"))
  }
}

object EmailAddress {

  private def apply(string: String): EmailAddress = new EmailAddress(string)

  def create(string: String): Either[Error, EmailAddress] = {
    string match {
      case "" => Left(Error("Email cannot be empty"))
      case nonEmptyString =>
        if (nonEmptyString.matches(".*@.*")) Right(new EmailAddress(nonEmptyString))
        else Left(Error("String does not match email pattern"))
    }
  }
}

object ZipCode {

  private def apply(string: String): ZipCode = new ZipCode(string)

  def create(string: String): Either[Error, ZipCode] = {
    if (string.matches("\\d{5}")) Right(new ZipCode(string))
    else Left(Error("Input is not 5-digit string"))
  }
}

object OrderId {

  private def apply(string: String): OrderId = new OrderId(string)

  def create(string: String): Either[Error, OrderId] = {
    if (string.isEmpty) {
      Left(Error("OrderId cannot be empty"))
    } else if (string.length > 50) {
      Left(Error("OrderId cannot be longer than 50 characters"))
    } else {
      Right(new OrderId(string))
    }
  }
}

object OrderLineId {

  private def apply(string: String): OrderLineId = new OrderLineId(string)

  def create(string: String): Either[Error, OrderLineId] = {
    if (string.isEmpty) {
      Left(Error("OrderId cannot be empty"))
    } else if (string.length > 50) {
      Left(Error("OrderId cannot be longer than 50 characters"))
    } else {
      Right(new OrderLineId(string))
    }
  }
}

case class UnvalidatedAddress(addressLine: String)

case class Address(addressLine: String)

case class UnvalidatedOrderLine(orderLineId: String, productCode: String, quantity: Double)

case class OrderLine(orderLineId: OrderLineId, productCode: ProductCode, quantity: ProductQuantity)

case class UnvalidatedCustomerInformation(emailAddress: String, address: UnvalidatedAddress)

case class UnvalidatedOrder(orderId: String, customerInformation: UnvalidatedCustomerInformation,
                            orderLines: List[UnvalidatedOrderLine])

case class Order(orderId: OrderId, customerInformation: CustomerInformation, orderLines: List[OrderLine])

object WidgetCode {

  private def apply(string: String): WidgetCode = new WidgetCode(string)

  def create(string: String): Either[Error, WidgetCode] = {
    if (string.matches("W\\d{4}")) Right(new WidgetCode(string))
    else Left(Error("Input is not a 5 chars string starting with W"))
  }
}

object GadgetCode {

  private def apply(string: String): GadgetCode = new GadgetCode(string)

  def create(string: String): Either[Error, GadgetCode] = {
    if (string.matches("G\\d{4}")) Right(new GadgetCode(string))
    else Left(Error("Input is not a 5 chars string starting with G"))
  }
}

object ProductCode {

  type CheckProductCodeExist = ProductCode => Boolean

  def create(string: String, checkProductCodeExist: CheckProductCodeExist): Either[Error, ProductCode] = {
    createWithoutVerifyingExistence(string) match {
      case Right(productCode) =>
        if (checkProductCodeExist(productCode)) Right(productCode)
        else Left(Error(s"Product code does not exist: $string"))
      case Left(error) => Left(error)
    }
  }

  private def createWithoutVerifyingExistence(string: String): Either[Error, ProductCode] = {
    if (string.startsWith("W")) WidgetCode.create(string)
    else if (string.startsWith("G")) GadgetCode.create(string)
    else Left(Error(s"Unrecognized product code format: $string"))
  }
}

object UnitQuantity {

  private def apply(integer: Int): UnitQuantity = new UnitQuantity(integer)

  def create(integer: Int): Either[Error, UnitQuantity] = {
    if (integer < 1) Left(Error("Input integer is less than 1"))
    else if (integer > 1000) Left(Error("Input integer is more than 1000"))
    else Right(new UnitQuantity(integer))
  }
}

object KilogramQuantity {

  private def apply(double: Double): KilogramQuantity = new KilogramQuantity(double)

  def create(double: Double): Either[Error, KilogramQuantity] = {
    if (double < 0.05D) Left(Error("Input double is less than 0.05"))
    else if (double > 100.00D) Left(Error("Input double is more than 100.00"))
    else Right(new KilogramQuantity(double))
  }
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

object Price {

  def value(price: Price): Double = price match {
    case Price(value) => value
  }

  private def apply(double: Double): Price = new Price(double)

  def create(double: Double): Either[Error, Price] = {
    if (double < 0.0D) Left(Error(s"Price cannot be less than 0.0"))
    else if (double > 1000.00D) Left(Error(s"Price cannot be more than 1000.00"))
    else Right(new Price(double))
  }

  def multiply(price: Price, quantity: ProductQuantity): Either[Error, Price] = {
    val decimalQuantity = ProductQuantity.decimalQuantity(quantity)
    Price.create(decimalQuantity * price.double)
  }
}

object BillingAmount {

  private def apply(double: Double): BillingAmount = new BillingAmount(double)

  def create(double: Double): Either[Error, BillingAmount] = {
    if (double < 0.0D) Left(Error(s"Billing amount cannot be less than 0.0"))
    else if (double > 10000.00D) Left(Error(s"Billing amount cannot be more than 10000.00"))
    else Right(new BillingAmount(double))
  }

  def total(prices: List[Price]): Either[Error, BillingAmount] = {
    val totalPrice = prices.map(Price.value).sum
    create(totalPrice)
  }
}

case class PricedOrderLine(orderLineId: OrderLineId, productCode: ProductCode,
                           productQuantity: ProductQuantity, price: Price)

case class PricedOrder(orderId: OrderId,
                       customerInformation: CustomerInformation,
                       pricedOrderLines: List[PricedOrderLine],
                       billingAmount: BillingAmount)

case class CustomerInformation(address: Address, emailAddress: EmailAddress)

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
