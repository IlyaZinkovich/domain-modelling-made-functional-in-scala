import io.github.ilyazinovich.dmmf.{EmailAddress, String50}

String50.create("ilya.zinkovich@github.io") match {
  case Right(String50(string)) => EmailAddress.create(string)
  case Left(error) => Left(error)
}
