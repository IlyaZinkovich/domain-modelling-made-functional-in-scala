package io.github.ilyazinovich.dmmf

import cats.data.NonEmptyList
import cats.data.Validated.Invalid
import io.github.ilyazinovich.dmmf.ProductCode.CheckProductCodeExist
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
    assertResult(BillingAmount.create(100.0).getOrElse(throw new RuntimeException))(pricedOrder.billingAmount)
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
