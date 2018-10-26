package io.github.ilyazinovich.dmmf

import cats.data.NonEmptyList
import cats.data.Validated.Invalid
import io.github.ilyazinovich.dmmf.AcknowledgeOrder.{CreateAcknowledgementLetter, SendAcknowledgement}
import io.github.ilyazinovich.dmmf.PlaceOrder.placeOrder
import io.github.ilyazinovich.dmmf.PriceOrder.GetProductPrice
import io.github.ilyazinovich.dmmf.ProductCode.CheckProductCodeExist
import io.github.ilyazinovich.dmmf.ValidateOrder.CheckAddressExist
import org.scalatest.{FlatSpec, Matchers}

class AppTest extends FlatSpec with Matchers {

  "ValidateOrder" should "invalidate order lines" in {
    val line = UnvalidatedOrderLine("1", "W123", -10.0)
    val lines = List(line)
    val checkProductCodeExist: CheckProductCodeExist = (_: ProductCode) => true
    assertResult(Invalid(NonEmptyList.of(Error("Input is not a 5 chars string starting with W"),
      Error("Unable to validate quantity because of invalid product code")))) {
      ValidateOrder.validateOrderLines(lines, checkProductCodeExist)
    }
  }

  "PriceOrder" should "calculate price for valid order" in {
    val order = createOrder()
    val result = PriceOrder.priceOrder(order, _ => Price.create(10.0).getOrElse(throw new RuntimeException))
    val pricedOrder = result.getOrElse(throw new RuntimeException)
    assert(BillingAmount.create(100.0).getOrElse(throw new RuntimeException) == pricedOrder.billingAmount)
  }

  "PlaceOrder" should "should produce all 3 events" in {
    val unvalidatedEmailAddress = "ilya@github.io"
    val address = UnvalidatedAddress("Kemperplatz 1")
    val customerInformation =
      UnvalidatedCustomerInformation(unvalidatedEmailAddress, address)
    val orderLine1 = UnvalidatedOrderLine("12345", "W1234", 2.0)
    val orderLine2 = UnvalidatedOrderLine("12346", "G1234", 10.0)
    val unvalidatedOrderId = "1"
    val unvalidatedOrder =
      UnvalidatedOrder(unvalidatedOrderId, customerInformation, List(orderLine1, orderLine2))
    val checkProductCodeExist: CheckProductCodeExist = _ => true
    val checkAddressExist: CheckAddressExist =
      unvalidatedAddress => Right(Address(unvalidatedAddress.addressLine))
    val getProductPrice: GetProductPrice =
      _ => Price.create(10).getOrElse(throw new RuntimeException)
    val acknowledgementLetter = AcknowledgementLetter("bla")
    val createAcknowledgementLetter: CreateAcknowledgementLetter = _ => acknowledgementLetter
    val sendAcknowledgement: SendAcknowledgement = _ => Sent
    val placeOrderEvents: Either[NonEmptyList[Error], List[PlaceOrderEvent]] = placeOrder(checkProductCodeExist, checkAddressExist, unvalidatedOrder,
      getProductPrice, createAcknowledgementLetter, sendAcknowledgement)
    placeOrderEvents match {
      case Right(List(billableOrderPlaced: BillableOrderPlaced, orderPlaced: OrderPlaced, acknowledgementSent: AcknowledgementSent)) => {
        assert(billableOrderPlaced.amountToBill.value == 120.0)
      }
    }
  }

  private def createOrder() = {
    val errorOrOrder = for {
      orderId <- OrderId.create("1").right
      emailAddress <- EmailAddress.create("ilya@github.io").right
    } yield Order(orderId, CustomerInformation(Address("Kemperplatz 1"), emailAddress), List(createOrderLine()))
    errorOrOrder.getOrElse(throw new RuntimeException)
  }

  private def createOrderLine() = {
    val orderLine = for {
      orderLineId <- OrderLineId.create("11").right
      productCode <- ProductCode.create("W1234", _ => true).right
      quantity <- ProductQuantity.create(productCode, 10.0)
    } yield OrderLine(orderLineId, productCode, quantity)
    orderLine.getOrElse(throw new RuntimeException)
  }
}
