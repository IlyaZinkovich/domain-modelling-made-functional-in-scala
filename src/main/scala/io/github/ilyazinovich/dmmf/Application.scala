package io.github.ilyazinovich.dmmf

import squants.{Dimensionless, Quantity}
import squants.mass.{Kilograms, Mass}

object Application extends App {

}

case class Error(cause: String)

case class String50 private(string: String)

object String50 {

  private def apply(string: String) = new String50(string)

  def create(string: String): Either[Error, String50] = {
    if (string.length <= 50) Right(new String50(string))
    else Left(Error("String is longer than 50 characters"))
  }
}

case class EmailAddress private(string: String)

object EmailAddress {

  private def apply(string: String): EmailAddress = new EmailAddress(string)

  def create(string: String): Either[Error, EmailAddress] = {
    string match {
      case "" => Left(Error("Email cannot be empty"))
      case nonEmptyString =>
        if (nonEmptyString.matches(".*@.*")) Right(new EmailAddress(nonEmptyString))
        else Left(Error("String does not match email pattern"))
    }
  }
}

case class ZipCode private(string: String)

object ZipCode {

  private def apply(string: String): ZipCode = new ZipCode(string)

  def create(string: String): Either[Error, ZipCode] = {
    if (string.matches("\\d{5}")) Right(new ZipCode(string))
    else Left(Error("Input is not 5-digit string"))
  }
}

case class OrderId private(string: String)

object OrderId {

  private def apply(string: String): OrderId = new OrderId(string)

  def create(string: String): Either[Error, OrderId] = {
    if (string.isEmpty) {
      Left(Error("OrderId cannot be empty"))
    } else if (string.length > 50) {
      Left(Error("OrderId cannot be longer than 50 characters"))
    } else {
      Right(new OrderId(string))
    }
  }
}

case class OrderLineId private(string: String)

object OrderLineId {

  private def apply(string: String): OrderLineId = new OrderLineId(string)

  def create(string: String): Either[Error, OrderLineId] = {
    if (string.isEmpty) {
      Left(Error("OrderId cannot be empty"))
    } else if (string.length > 50) {
      Left(Error("OrderId cannot be longer than 50 characters"))
    } else {
      Right(new OrderLineId(string))
    }
  }
}

sealed trait ProductCode

case class WidgetCode private(string: String) extends ProductCode

object WidgetCode {

  private def apply(string: String): WidgetCode = new WidgetCode(string)

  def create(string: String): Either[Error, WidgetCode] = {
    if (string.matches("W\\d{4}")) Right(new WidgetCode(string))
    else Left(Error("Input is not a 5 chars string starting with W"))
  }
}

case class GadgetCode private(string: String) extends ProductCode

object GadgetCode {

  private def apply(string: String): GadgetCode = new GadgetCode(string)

  def create(string: String): Either[Error, GadgetCode] = {
    if (string.matches("G\\d{4}")) Right(new GadgetCode(string))
    else Left(Error("Input is not a 5 chars string starting with G"))
  }
}

object ProductCode {

  def create(string: String): Either[Error, ProductCode] = {
    if (string.startsWith("W")) WidgetCode.create(string)
    else if (string.startsWith("G")) GadgetCode.create(string)
    else Left(Error(s"Unrecognized product code format: $string"))
  }
}

sealed trait ProductQuantity

case class UnitQuantity private(integer: Int) extends ProductQuantity

object UnitQuantity {

  private def apply(integer: Int): UnitQuantity = new UnitQuantity(integer)

  def create(integer: Integer): Either[Error, UnitQuantity] = {
    if (integer < 1) Left(Error("Input integer is less than 1"))
    else if (integer > 1000) Left(Error("Input integer is more than 1000"))
    else Right(new UnitQuantity(integer))
  }
}

case class KilogramQuantity private(double: Double) extends ProductQuantity

object KilogramQuantity {

  private def apply(double: Double): KilogramQuantity = new KilogramQuantity(double)

  def create(double: Double): Either[Error, KilogramQuantity] = {
    if (double < 0.05D) Left(Error("Input double is less than 0.05"))
    else if (double > 100.00D) Left(Error("Input double is more than 100.00"))
    else Right(new KilogramQuantity(double))
  }
}
