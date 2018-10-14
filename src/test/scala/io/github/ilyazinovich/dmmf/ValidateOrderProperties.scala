package io.github.ilyazinovich.dmmf

import cats.data.Validated.{Invalid, Valid}
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalatest.PropSpec
import org.scalatest.prop.Checkers

class ValidateOrderProperties extends PropSpec with Checkers {

  private val genAddress: Gen[UnvalidatedAddress] = Gen.alphaNumStr.map(UnvalidatedAddress)

  private val genCustomerInformation: Gen[UnvalidatedCustomerInformation] = for {
    emailAddress <- for (a <- Gen.alphaNumStr; b <- Gen.alphaNumStr) yield a + "@" + b
    address <- genAddress
  } yield UnvalidatedCustomerInformation(emailAddress, address)

  private val genWidget: Gen[(String, Double)] = for {
    code <- numStrGen(4).map("W" + _)
    q <- Gen.choose(1, 1000)
  } yield (code, q)

  private val genGadget: Gen[(String, Double)] = for {
    code <- numStrGen(4).map("G" + _)
    q <- Gen.choose(0.05, 100.0)
  } yield (code, q)

  private val genOrderLine = for {
    orderLineId <- strGen(50)
    (productCode, quantity) <- Gen.oneOf(genWidget, genGadget)
  } yield UnvalidatedOrderLine(orderLineId, productCode, quantity)

  private val genUnvalidatedOrder: Gen[UnvalidatedOrder] = for {
    orderId <- strGen(50)
    customerInformation <- genCustomerInformation
    orderLines <- Gen.listOf(genOrderLine)
  } yield UnvalidatedOrder(orderId, customerInformation, orderLines)

  private val genCorrectAddress =
    (unvalidatedAddress: UnvalidatedAddress) => Right(Address(unvalidatedAddress.addressLine))

  property("validate") {
    check(forAll(genUnvalidatedOrder) { unvalidatedOrder =>
      ValidateOrder.validateOrder(_ => true, genCorrectAddress, unvalidatedOrder) match {
        case Valid(Order(OrderId(orderId), CustomerInformation(address, emailAddress), orderLines)) =>
          orderId == unvalidatedOrder.orderId
        case Invalid(_) =>
          false
      }
    })
  }

  def numStrGen(n: Int): Gen[String] = Gen.listOfN(n, Gen.numChar).map(_.mkString)
  def strGen(n: Int): Gen[String] = Gen.choose(1, n).flatMap(n => Gen.listOfN(n, Gen.numChar).map(_.mkString))

}
