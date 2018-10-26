package io.github.ilyazinovich.dmmf

import cats.data.Validated.Valid
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalatest.PropSpec
import org.scalatest.prop.Checkers

class ValidateOrderProperties extends PropSpec with Checkers {

  private val genAddress: Gen[UnvalidatedAddress] = Gen.alphaNumStr.map(UnvalidatedAddress)

  private val genEmailAddress: Gen[String] = for {
    prefix <- Gen.alphaNumStr
    suffix <- Gen.alphaNumStr
  } yield prefix + "@" + suffix

  private val genCustomerInformation: Gen[UnvalidatedCustomerInformation] = for {
    emailAddress <- genEmailAddress
    address <- genAddress
  } yield UnvalidatedCustomerInformation(emailAddress, address)

  private def genString(charGenerator: Gen[Char], length: Int): Gen[String] =
    Gen.listOfN(length, charGenerator).map(_.mkString)

  private def genAlphaNumericString(length: Int): Gen[String] = genString(Gen.alphaNumChar, length)

  private def genNumericString(length: Int): Gen[String] = genString(Gen.numChar, length)

  private val genWidget: Gen[(String, Double)] = for {
    widgetCode <- genNumericString(4).map("W" + _)
    widgetQuantity <- Gen.chooseNum(1, 1000)
  } yield (widgetCode, widgetQuantity)

  private val genGadget: Gen[(String, Double)] = for {
    gadgetCode <- genNumericString(4).map("G" + _)
    gadgetQuantity <- Gen.choose(0.05, 100.0)
  } yield (gadgetCode, gadgetQuantity)

  private val genOrderLine = for {
    orderLineId <- genAlphaNumericString(50)
    (productCode, quantity) <- Gen.oneOf(genWidget, genGadget)
  } yield UnvalidatedOrderLine(orderLineId, productCode, quantity)

  private val genValidUnvalidatedOrder: Gen[UnvalidatedOrder] = for {
    orderId <- genAlphaNumericString(50)
    customerInformation <- genCustomerInformation
    orderLines <- Gen.listOf(genOrderLine)
  } yield UnvalidatedOrder(orderId, customerInformation, orderLines)

  private val genCorrectAddress =
    (unvalidatedAddress: UnvalidatedAddress) => Right(Address(unvalidatedAddress.addressLine))

  property("validation doesn't change input data") {
    check(forAll(genValidUnvalidatedOrder) { unvalidatedOrder =>
      ValidateOrder.validateOrder(_ => true, genCorrectAddress, unvalidatedOrder) match {
        case Valid(Order(OrderId(orderId), CustomerInformation(address, emailAddress), orderLines)) =>
          orderId == unvalidatedOrder.orderId &&
            address.addressLine == unvalidatedOrder.customerInformation.address.addressLine &&
            emailAddress.value == unvalidatedOrder.customerInformation.emailAddress &&
            compareOrderLines(orderLines, unvalidatedOrder.orderLines)
        case _ => false
      }
    })
  }

  private def compareOrderLines(orderLines: List[OrderLine],
                                unvalidatedOrderLines: List[UnvalidatedOrderLine]): Boolean = {
    (orderLines zip unvalidatedOrderLines) forall compareIndividualOrderLines
  }

  def compareIndividualOrderLines(orderLines: (OrderLine, UnvalidatedOrderLine)): Boolean = {
    orderLines match {
      case (orderLine, unvalidatedOrderLine) =>
        orderLine.orderLineId.value == unvalidatedOrderLine.orderLineId &&
          ProductCode.value(orderLine.productCode) == unvalidatedOrderLine.productCode &&
          ProductQuantity.decimalQuantity(orderLine.quantity) == unvalidatedOrderLine.quantity
    }
  }
}
