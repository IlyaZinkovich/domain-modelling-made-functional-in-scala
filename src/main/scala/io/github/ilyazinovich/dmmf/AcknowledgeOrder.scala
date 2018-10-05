package io.github.ilyazinovich.dmmf

class AcknowledgeOrder {

  type CreateAcknowledgementLetter = PricedOrder => AcknowledgementLetter
  type SendAcknowledgement = Acknowledgement => AcknowledgementStatus

  def acknowledgeOrder(createAcknowledgementLetter: CreateAcknowledgementLetter,
                       sendAcknowledgement: SendAcknowledgement,
                       pricedOrder: PricedOrder) = {
    val acknowledgementLetter = createAcknowledgementLetter(pricedOrder)
    Acknowledgement(pricedOrder.customerInformation.emailAddress, acknowledgementLetter)
  }
}

sealed trait AcknowledgementStatus
case class Sent() extends AcknowledgementStatus
case class NotSent() extends AcknowledgementStatus

case class AcknowledgementLetter(content: String)
case class Acknowledgement(emailAddress: EmailAddress, content: AcknowledgementLetter)
