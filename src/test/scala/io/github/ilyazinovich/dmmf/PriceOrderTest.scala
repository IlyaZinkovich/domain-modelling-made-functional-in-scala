package io.github.ilyazinovich.dmmf

import cats.data.NonEmptyList
import io.github.ilyazinovich.dmmf.PriceOrder.GetProductPrice
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalatest.PropSpec
import org.scalatest.prop.Checkers

class PriceOrderTest extends PropSpec with Checkers {

  private def genString(charGenerator: Gen[Char], length: Int): Gen[String] =
    Gen.listOfN(length, charGenerator).map(_.mkString)

  private def genAlphaNumericString(length: Int): Gen[String] = genString(Gen.alphaNumChar, length)

  private def genNumericString(length: Int): Gen[String] = genString(Gen.numChar, length)

  private val genOrderId: Gen[OrderId] =
    genAlphaNumericString(50).map(OrderId.create).map(_.getOrElse(throw GeneratorException))

  private val genEmailAddress: Gen[EmailAddress] = for {
    prefix <- Gen.alphaNumStr
    suffix <- Gen.alphaNumStr
  } yield EmailAddress.create(prefix + "@" + suffix).getOrElse(throw GeneratorException)

  private val genCustomerInformation: Gen[CustomerInformation] = for {
    address <- Gen.alphaNumStr.map(Address)
    emailAddress <- genEmailAddress
  } yield CustomerInformation(address, emailAddress)

  private val genWidget: Gen[(String, Double)] = for {
    widgetCode <- genNumericString(4).map("W" + _)
    widgetQuantity <- Gen.chooseNum(1, 1000)
  } yield (widgetCode, widgetQuantity)

  private val genGadget: Gen[(String, Double)] = for {
    gadgetCode <- genNumericString(4).map("G" + _)
    gadgetQuantity <- Gen.choose(0.05, 100.0)
  } yield (gadgetCode, gadgetQuantity)

  private val genOrderLine: Gen[OrderLine] = for {
    orderLineId <- genAlphaNumericString(50).map(OrderLineId.create)
      .map(_.getOrElse(throw GeneratorException))
    (productCodeString, quantityDouble) <- Gen.oneOf(genWidget, genGadget)
  } yield {
    val productCode = ProductCode.create(productCodeString, _ => true).getOrElse(throw GeneratorException)
    val quantity = ProductQuantity.create(productCode, quantityDouble).getOrElse(throw GeneratorException)
    OrderLine(orderLineId, productCode, quantity)
  }

  private val genOrder: Gen[Order] = for {
    orderId <- genOrderId
    customerInformation <- genCustomerInformation
    orderLines <- Gen.listOf(genOrderLine)
  } yield Order(orderId, customerInformation, orderLines)

  private val genGetProductPrice: Gen[GetProductPrice] = for {
    price <- Gen.choose(0.1D, 1000.0D).map(Price.create).map(_.getOrElse(throw GeneratorException))
  } yield new GetProductPrice {
    override def apply(productCode: ProductCode): Price = price
  }

  property("pricing accurately calculates billing amount") {
    check(forAll(genOrder, genGetProductPrice) { (order, getProductPrice) =>
      PriceOrder.priceOrder(order, getProductPrice) match {
        case Right(PricedOrder(_, _, pricedOrderLines, billingAmount)) =>
          val totalOrderLinesPrice =
            unvalidatedOrderLinesPriceTotal(order, getProductPrice)
          val totalPricedOrderLinesPrice =
            pricedOrderLines.map(pricedOrderLine => pricedOrderLine.price.double).sum
          totalPricedOrderLinesPrice == totalOrderLinesPrice &&
            totalPricedOrderLinesPrice == billingAmount.double
        case Left(errors) =>
          errors match {
            case NonEmptyList(TooLargeBillingAmount(billingAmount, limit), Nil) =>
              val totalOrderLinesPrice =
                unvalidatedOrderLinesPriceTotal(order, getProductPrice)
              totalOrderLinesPrice == billingAmount && billingAmount > limit
            case NonEmptyList(TooHighPrice(price, limit), _) => price > limit
            case _ => false
          }
        case _ => true
      }
    })
  }

  private def unvalidatedOrderLinesPriceTotal(order: Order, getProductPrice: GetProductPrice) = {
    order.orderLines.map(orderLine => orderLine.productCode)
      .map(getProductPrice).map(price => price.double).sum
  }

  case object GeneratorException extends RuntimeException

}
