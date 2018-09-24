package io.github.ilyazinovich.dmmf

import cats.Apply
import cats.data.Validated._
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.traverse._
import io.github.ilyazinovich.dmmf.ProductCode.CheckProductCodeExist

object PlaceOrder {

  type CheckAddressExist = UnvalidatedAddress => Either[Error, Address]
  type ValidatedOrderLinePart[T] = ValidatedNel[Error, T]

  def validateOrder(checkProductCodeExists: CheckProductCodeExist,
                    checkAddressExist: CheckAddressExist,
                    unvalidatedOrder: UnvalidatedOrder): Unit = {
  }

  def validateOrderId(orderId: String): Validated[Error, OrderId] = {
    OrderId.create(orderId).toValidated
  }

  def validateAddress(address: UnvalidatedAddress, checkAddressExist: CheckAddressExist): Validated[Error, Address] = {
    checkAddressExist(address).toValidated
  }

  def validateOrderLines(orderLines: List[UnvalidatedOrderLine],
                         checkProductCodeExist: CheckProductCodeExist): ValidatedNel[Error, List[OrderLine]] = {
    orderLines.traverse[ValidatedOrderLinePart, OrderLine] { orderLine =>
      val validatedOrderLineId = OrderLineId.create(orderLine.orderLineId).toValidatedNel
      val validatedProductCode = ProductCode.create(orderLine.productCode, checkProductCodeExist).toValidatedNel
      val validatedProductQuantity = validatedProductCode match {
        case Valid(productCode) => ProductQuantity.create(productCode, orderLine.quantity).toValidatedNel
        case Invalid(_) => Invalid(NonEmptyList.of(Error("Unable to validate quantity because of invalid product code")))
      }
      Apply[ValidatedOrderLinePart].map3(validatedOrderLineId, validatedProductCode, validatedProductQuantity) {
        case (orderLineId, productCode, productQuantity) => OrderLine(orderLineId, productCode, productQuantity)
      }
    }
  }
}

case class UnvalidatedOrder(orderId: String, address: UnvalidatedAddress, orderLines: List[UnvalidatedOrderLine])

case class UnvalidatedAddress(addressLine: String)

case class UnvalidatedOrderLine(orderLineId: String, productCode: String, quantity: Double)

case class Order(orderId: OrderId, address: Address, orderLines: List[OrderLine])

case class Address(addressLine: String)

case class OrderLine(orderLineId: OrderLineId, productCode: ProductCode, quantity: ProductQuantity)

