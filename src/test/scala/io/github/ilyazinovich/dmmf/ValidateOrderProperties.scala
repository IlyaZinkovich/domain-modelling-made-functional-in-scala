package io.github.ilyazinovich.dmmf

import cats.data.Validated.Valid
import io.github.ilyazinovich.dmmf.ProductCode.CheckProductCodeExist
import io.github.ilyazinovich.dmmf.ValidateOrder.CheckAddressExist
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.PropSpec
import org.scalatest.prop.Checkers

class ValidateOrderProperties extends PropSpec with Checkers {

  private val genAddress: Gen[UnvalidatedAddress] = for {
    addressLine <- Gen.alphaNumStr
  } yield UnvalidatedAddress(addressLine)

  private val genCustomerInformation: Gen[UnvalidatedCustomerInformation] = for {
    emailAddress <- Gen.oneOf(Gen.alphaNumStr, Gen.alphaNumStr suchThat (_.contains("@")))
    address <- genAddress
  } yield UnvalidatedCustomerInformation(emailAddress, address)

  private val genWidgetCode: Gen[String] =
    Gen.alphaNumStr suchThat (code => code.startsWith("W") && code.length == 5)

  private val genGadgetCode: Gen[String] =
    Gen.alphaNumStr suchThat (code => code.startsWith("G") && code.length == 5)

  private val genOrderLine = for {
    orderLineId <- Gen.alphaNumStr
    productCode <- Gen.oneOf(Gen.alphaNumStr, genWidgetCode, genGadgetCode)
    quantity <- Arbitrary.arbitrary[Double]
  } yield UnvalidatedOrderLine(orderLineId, productCode, quantity)

  private val genUnvalidatedOrder: Gen[UnvalidatedOrder] = for {
    orderId <- Gen.alphaNumStr
    customerInformation <- genCustomerInformation
    orderLines <- Gen.listOf(genOrderLine)
  } yield UnvalidatedOrder(orderId, customerInformation, orderLines)

  private val genCheckProductCodeExist: Gen[CheckProductCodeExist] = for {
    bool <- Arbitrary.arbBool.arbitrary
  } yield (_: ProductCode) => bool

  private val genError: Gen[CheckAddressExist] = for {
    errorMessage <- Gen.alphaNumStr
  } yield (_: UnvalidatedAddress) => Left(Error(errorMessage))

  private val genLeftAddress: Gen[CheckAddressExist] =
    (unvalidatedAddress: UnvalidatedAddress) => Right(Address(unvalidatedAddress.addressLine))

  private val genCheckAddressExist: Gen[CheckAddressExist] = Gen.oneOf(genLeftAddress, genError)

  property("validate") {
    check(forAll(genUnvalidatedOrder, genCheckProductCodeExist, genCheckAddressExist) {
      (unvalidatedOrder, checkProductCodeExist, checkAddressExist) =>
        ValidateOrder.validateOrder(checkProductCodeExist, checkAddressExist, unvalidatedOrder) match {
          case Valid(Order(OrderId(orderId), CustomerInformation(address, emailAddress), orderLines)) => {
            unvalidatedOrder.orderId == orderId
          }
          case _ => false
        }
    })
  }
}
