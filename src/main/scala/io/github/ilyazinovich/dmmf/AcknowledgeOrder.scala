package io.github.ilyazinovich.dmmf

class AcknowledgeOrder {

  type CreateAcknowledgementLetter = PricedOrder => AcknowledgementLetter
  type SendAcknowledgement = Acknowledgement => AcknowledgementStatus

  def acknowledgeOrder(createAcknowledgementLetter: CreateAcknowledgementLetter,
                       sendAcknowledgement: SendAcknowledgement,
                       pricedOrder: PricedOrder): Option[AcknowledgementSent] = {
    val acknowledgementLetter = createAcknowledgementLetter(pricedOrder)
    val customerEmailAddress = pricedOrder.customerInformation.emailAddress
    val acknowledgement = Acknowledgement(customerEmailAddress, acknowledgementLetter)
    sendAcknowledgement(acknowledgement) match {
      case Sent => Some(AcknowledgementSent(pricedOrder.orderId, customerEmailAddress))
      case NotSent => None
    }
  }
}

sealed trait AcknowledgementStatus
case object Sent extends AcknowledgementStatus
case object NotSent extends AcknowledgementStatus

case class AcknowledgementLetter(content: String)
case class Acknowledgement(emailAddress: EmailAddress, content: AcknowledgementLetter)

case class AcknowledgementSent(orderId: OrderId, emailAddress: EmailAddress)
