package io.github.ilyazinovich.dmmf

import cats.data.NonEmptyList
import cats.instances.list._
import cats.syntax.either._

object PriceOrder {

  type GetProductPrice = ProductCode => Price

  def priceOrder(order: Order, getProductPrice: GetProductPrice): Either[NonEmptyList[PricingError], PricedOrder] = {
    calculateOrderLinesPrice(order.orderLines, getProductPrice).flatMap { pricedOrderLines =>
      val prices = pricedOrderLines.map(_.price)
      BillingAmount.total(prices).bimap(
        NonEmptyList.of(_),
        billingAmount => PricedOrder(order.orderId, order.customerInformation, pricedOrderLines, billingAmount)
      )
    }
  }

  private def calculateOrderLinesPrice(orderLines: List[OrderLine],
                                       getProductPrice: GetProductPrice) = {
    val pricedLines: List[Either[PricingError, PricedOrderLine]] = orderLines.map {
      case OrderLine(orderLineId, productCode, quantity) =>
        val orderLinePrice = Price.multiply(getProductPrice(productCode), quantity)
        orderLinePrice.map(p => PricedOrderLine(orderLineId, productCode, quantity, p))
    }
    val accumulator: Either[NonEmptyList[PricingError], List[PricedOrderLine]] = Right(List.empty)
    pricedLines.foldLeft(accumulator)(reduce)
  }

  private def reduce(accumulator: Either[NonEmptyList[PricingError], List[PricedOrderLine]],
                     orderLinePriceCalculationResult: Either[PricingError, PricedOrderLine]) = {
    orderLinePriceCalculationResult.bimap(NonEmptyList.one, List(_)) combine accumulator
  }
}
