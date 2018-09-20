package io.github.ilyazinovich.dmmf

object Application extends App {

}

case class Error(cause: String)

case class String50 private(value: String)

object String50 {

  private def apply(value: String) = new String50(value)

  def create(value: String): Either[Error, String50] = {
    if (value.length <= 50) Right(new String50(value))
    else Left(Error("String is longer than 50 characters"))
  }
}

case class EmailAddress private(value: String)

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
