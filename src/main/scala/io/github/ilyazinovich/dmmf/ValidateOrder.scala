package io.github.ilyazinovich.dmmf

import cats.Apply
import cats.data.Validated._
import cats.data.{NonEmptyList, ValidatedNel}
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.traverse._
import io.github.ilyazinovich.dmmf.ProductCode.CheckProductCodeExist

object ValidateOrder {

  type CheckAddressExist = UnvalidatedAddress => Either[Error, Address]
  type ValidatedOrderLinePart[T] = ValidatedNel[Error, T]

  def validateOrder(checkProductCodeExists: CheckProductCodeExist,
                    checkAddressExist: CheckAddressExist,
                    unvalidatedOrder: UnvalidatedOrder): ValidatedNel[Error, Order] = {
    apply(
      apply(
        apply(
          lift((Order.apply _).curried), validateOrderId(unvalidatedOrder.orderId)
        ), validateAddress(unvalidatedOrder.address, checkAddressExist)
      ), validateOrderLines(unvalidatedOrder.orderLines, checkProductCodeExists)
    )
  }

  def validateOrderId(orderId: String): ValidatedNel[Error, OrderId] = {
    OrderId.create(orderId).toValidatedNel
  }

  def validateAddress(address: UnvalidatedAddress, checkAddressExist: CheckAddressExist): ValidatedNel[Error, Address] = {
    checkAddressExist(address).toValidatedNel
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
      liftAndApply(validatedOrderLineId, validatedProductCode, validatedProductQuantity)
    }
  }

  private def liftAndApply(validatedOrderLineId: ValidatedNel[Error, OrderLineId], validatedProductCode: ValidatedNel[Error, ProductCode], validatedProductQuantity: ValidatedNel[Error, ProductQuantity]) = {
    apply(
      apply(
        apply(
          lift((OrderLine.apply _).curried), validatedOrderLineId
        ), validatedProductCode
      ), validatedProductQuantity
    )
  }

  private def mapAndApply(validatedOrderLineId: ValidatedNel[Error, OrderLineId], validatedProductCode: ValidatedNel[Error, ProductCode], validatedProductQuantity: ValidatedNel[Error, ProductQuantity]) = {
    apply(
      apply(
        map((OrderLine.apply _).curried, validatedOrderLineId),
        validatedProductCode
      ),
      validatedProductQuantity
    )
  }

  private def catsApplicative(validatedOrderLineId: ValidatedNel[Error, OrderLineId], validatedProductCode: ValidatedNel[Error, ProductCode], validatedProductQuantity: ValidatedNel[Error, ProductQuantity]) = {
    Apply[ValidatedOrderLinePart].map3(validatedOrderLineId, validatedProductCode, validatedProductQuantity) {
      case (orderLineId, productCode, productQuantity) => OrderLine(orderLineId, productCode, productQuantity)
    }
  }

  def lift[A, B](func: A => B): ValidatedNel[Error, A => B] = {
    Valid.apply(func)
  }

  def map[A, B](func: A => B, value: ValidatedNel[Error, A]): ValidatedNel[Error, B] = {
    value match {
      case Valid(result) => Valid(func.apply(result))
      case Invalid(errors) => Invalid(errors)
    }
  }

  def apply[A, B](func: ValidatedNel[Error, A => B], value: ValidatedNel[Error, A]): ValidatedNel[Error, B] = {
    (func, value) match {
      case (Valid(validFunc), Valid(valueResult)) => Valid(validFunc.apply(valueResult))
      case (Valid(_), Invalid(valueErrors)) => Invalid(valueErrors)
      case (Invalid(funcErrors), Valid(_)) => Invalid(funcErrors)
      case (Invalid(funcErrors), Invalid(valueErrors)) => Invalid(funcErrors.concatNel(valueErrors))
    }
  }
}
