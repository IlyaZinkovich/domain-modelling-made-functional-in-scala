package io.github.ilyazinovich.dmmf

import cats.data.ValidatedNel
import io.github.ilyazinovich.dmmf.AcknowledgeOrder.{CreateAcknowledgementLetter, SendAcknowledgement, acknowledgeOrder}
import io.github.ilyazinovich.dmmf.PriceOrder.{GetProductPrice, priceOrder}
import io.github.ilyazinovich.dmmf.ProductCode.CheckProductCodeExist
import io.github.ilyazinovich.dmmf.ValidateOrder.{CheckAddressExist, validateOrder}

object PlaceOrder {

  def placeOrder(checkProductCodeExist: CheckProductCodeExist,
                 checkAddressExist: CheckAddressExist,
                 unvalidatedOrder: UnvalidatedOrder,
                 getProductPrice: GetProductPrice,
                 createAcknowledgementLetter: CreateAcknowledgementLetter,
                 sendAcknowledgement: SendAcknowledgement) = {
    val orderValidationResult: ValidatedNel[Error, Order] =
      validateOrder(checkProductCodeExist, checkAddressExist, unvalidatedOrder)
    for {
      order <- orderValidationResult.toEither.right
      pricedOrder <- priceOrder(order, getProductPrice).right
      acknowledgment <- acknowledgeOrder(createAcknowledgementLetter, sendAcknowledgement, pricedOrder)
    } yield acknowledgment
  }
}
