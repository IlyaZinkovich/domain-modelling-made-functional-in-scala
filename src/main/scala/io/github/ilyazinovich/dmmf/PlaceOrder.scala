package io.github.ilyazinovich.dmmf

import cats.data.{NonEmptyList, ValidatedNel}
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
                 sendAcknowledgement: SendAcknowledgement): Either[NonEmptyList[Error], List[PlaceOrderEvent]] = {
    val orderValidationResult: ValidatedNel[Error, Order] =
      validateOrder(checkProductCodeExist, checkAddressExist, unvalidatedOrder)
    for {
      order <- orderValidationResult.toEither
      pricedOrder <- priceOrder(order, getProductPrice)
    } yield createEvents(acknowledgeOrder(createAcknowledgementLetter, sendAcknowledgement, pricedOrder), pricedOrder)
  }

  def createEvents(optionalAcknowledgementSetEvent: Option[AcknowledgementSent],
                   pricedOrder: PricedOrder): List[PlaceOrderEvent] = {
    val billableOrderPlaced = BillableOrderPlaced(pricedOrder.orderId,
      pricedOrder.customerInformation.address, pricedOrder.billingAmount)
    val orderPlaced = OrderPlaced(pricedOrder)
    billableOrderPlaced :: orderPlaced :: optionalAcknowledgementSetEvent.toList
  }
}
