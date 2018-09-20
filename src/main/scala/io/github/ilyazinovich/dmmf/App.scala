package io.github.ilyazinovich.dmmf

class App {

}

case class Error(cause: String)

case class String50 private (value: String)

object String50 {

  private def apply(value: String) = new String50(value)

  def unapply(arg: String50): Option[String] = Some(arg.value)

  def create(value: String): Either[Error, Option[String50]] = {
    value match {
      case "" => Right(None)
      case string =>
        if (string.length <= 50) Right(Some(new String50(string)))
        else Left(Error("String is longer than 50 characters"))
    }
  }
}

case class EmailAddress private (value: String)

object EmailAddress {

  private def apply(value: String): EmailAddress = new EmailAddress(value)

  def unapply(arg: EmailAddress): Option[String] = Some(arg.value)

  def create(value: String): Either[Error, EmailAddress] = {
    value match {
      case "" => Left(Error("Email cannot be empty"))
      case string =>
        if (string.matches(".*@.*")) Right(new EmailAddress(string))
        else Left(Error("String does not match email pattern"))
    }
  }
}
