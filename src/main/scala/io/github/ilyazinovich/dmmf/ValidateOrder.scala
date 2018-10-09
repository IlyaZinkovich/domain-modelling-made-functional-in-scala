package io.github.ilyazinovich.dmmf

import cats.Applicative
import cats.data.Validated._
import cats.data.{NonEmptyList, ValidatedNel}
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.traverse._
import io.github.ilyazinovich.dmmf.ProductCode.CheckProductCodeExist

import scala.language.higherKinds

object ValidateOrder {

  type CheckAddressExist = UnvalidatedAddress => Either[Error, Address]
  type ValidationResult[T] = ValidatedNel[Error, T]

  def validateOrder(checkProductCodeExists: CheckProductCodeExist,
                    checkAddressExist: CheckAddressExist,
                    unvalidatedOrder: UnvalidatedOrder): ValidatedNel[Error, Order] = {
    val orderId = validateOrderId(unvalidatedOrder.orderId)
    val customerInformation = validateCustomerInformation(unvalidatedOrder.customerInformation, checkAddressExist)
    val orderLines = validateOrderLines(unvalidatedOrder.orderLines, checkProductCodeExists)
    apply(apply(apply(pure((Order.apply _).curried), orderId), customerInformation), orderLines)
  }

  private def validateOrderId(orderId: String): ValidatedNel[Error, OrderId] = {
    OrderId.create(orderId).toValidatedNel
  }

  private def validateCustomerInformation(customerInformation: UnvalidatedCustomerInformation,
                                          checkAddressExist: CheckAddressExist): ValidatedNel[Error, CustomerInformation] = {
    val addressValidationResult: ValidatedNel[Error, Address] = checkAddressExist(customerInformation.address).toValidatedNel
    val emailValidationResult: ValidatedNel[Error, EmailAddress] = EmailAddress.create(customerInformation.emailAddress).toValidatedNel
    apply(apply(pure((CustomerInformation.apply _).curried), addressValidationResult), emailValidationResult)
  }

  def validateOrderLines(orderLines: List[UnvalidatedOrderLine],
                         checkProductCodeExist: CheckProductCodeExist): ValidationResult[List[OrderLine]] = {
    orderLines.traverse[ValidationResult, OrderLine] { orderLine =>
      val validatedOrderLineId = OrderLineId.create(orderLine.orderLineId).toValidatedNel
      val validatedProductCode = ProductCode.create(orderLine.productCode, checkProductCodeExist).toValidatedNel
      val validatedProductQuantity = validatedProductCode match {
        case Valid(productCode) => ProductQuantity.create(productCode, orderLine.quantity).toValidatedNel
        case Invalid(_) => Invalid(NonEmptyList.of(Error("Unable to validate quantity because of invalid product code")))
      }
      pureAndApply(validatedOrderLineId, validatedProductCode, validatedProductQuantity)
    }
  }

  private def pureAndApply(orderLineId: ValidatedNel[Error, OrderLineId],
                           productCode: ValidatedNel[Error, ProductCode],
                           productQuantity: ValidatedNel[Error, ProductQuantity]) = {
    apply(apply(apply(pure((OrderLine.apply _).curried), orderLineId), productCode), productQuantity)
  }

  private def catsApplicative(validatedOrderLineId: ValidationResult[OrderLineId],
                              validatedProductCode: ValidationResult[ProductCode],
                              validatedProductQuantity: ValidationResult[ProductQuantity]) = {
    (validatedOrderLineId, validatedProductCode, validatedProductQuantity).mapN {
      case (orderLineId, productCode, productQuantity) => OrderLine(orderLineId, productCode, productQuantity)
    }
  }

  private def catsPureAndApply(validatedOrderLineId: ValidationResult[OrderLineId],
                               validatedProductCode: ValidationResult[ProductCode],
                               validatedProductQuantity: ValidationResult[ProductQuantity]): ValidationResult[OrderLine] = {
    (OrderLine.apply _).curried <&> validatedOrderLineId <*> validatedProductCode <*> validatedProductQuantity
  }

  private implicit class ApplicativeMap[A, B](function: A => B) {
    def <&>[T[_] : Applicative](applicative: T[A]): T[B] = {
      function.pure[T] <*> applicative
    }
  }

  private def pure[A, B](func: A => B): ValidatedNel[Error, A => B] = {
    Valid.apply(func)
  }

  private def apply[A, B](func: ValidatedNel[Error, A => B], value: ValidatedNel[Error, A]): ValidatedNel[Error, B] = {
    (func, value) match {
      case (Valid(validFunc), Valid(valueResult)) => Valid(validFunc.apply(valueResult))
      case (Valid(_), Invalid(valueErrors)) => Invalid(valueErrors)
      case (Invalid(funcErrors), Valid(_)) => Invalid(funcErrors)
      case (Invalid(funcErrors), Invalid(valueErrors)) => Invalid(funcErrors.concatNel(valueErrors))
    }
  }
}
