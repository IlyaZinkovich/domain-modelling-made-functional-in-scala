package io.github.ilyazinovich.dmmf

import cats.data.NonEmptyList

object PriceOrder {

  type GetProductPrice = ProductCode => Price

  def priceOrder(order: Order, getProductPrice: GetProductPrice): Either[NonEmptyList[Error], PricedOrder] = {
    calculateOrderLinesPrice(order.orderLines, getProductPrice).right.flatMap { pricedOrderLines =>
      val prices = pricedOrderLines.map(pricedOrderLine => pricedOrderLine.price)
      BillingAmount.total(prices) match {
        case Left(error) => Left(NonEmptyList.of(error))
        case Right(billingAmount) =>
          Right(PricedOrder(order.orderId, order.address, pricedOrderLines, billingAmount))
      }
    }
  }

  private def calculateOrderLinesPrice(orderLines: List[OrderLine],
                                       getProductPrice: GetProductPrice) = {
    val pricedLines: List[Either[Error, PricedOrderLine]] = orderLines.map {
      case OrderLine(orderLineId, productCode, quantity) =>
        val orderLinePrice = Price.multiply(getProductPrice(productCode), quantity)
        orderLinePrice.right.map(p => PricedOrderLine(orderLineId, productCode, quantity, p))
    }
    val accumulator: Either[NonEmptyList[Error], List[PricedOrderLine]] = Right(List.empty)
    pricedLines.foldRight(accumulator)(reduce)
  }

  private def reduce(orderLinePriceCalculationResult: Either[Error, PricedOrderLine],
                     accumulator: Either[NonEmptyList[Error], List[PricedOrderLine]]) = {
    (orderLinePriceCalculationResult, accumulator) match {
      case (Left(error), Left(errorList)) => Left(error :: errorList)
      case (Left(error), Right(_)) => Left(NonEmptyList.of(error))
      case (Right(_), Left(errorList)) => Left(errorList)
      case (Right(pricedOrderLine), Right(pricedOrderLines)) =>
        Right(pricedOrderLine :: pricedOrderLines)
    }
  }
}
