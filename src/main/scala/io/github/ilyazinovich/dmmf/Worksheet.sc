import io.github.ilyazinovich.dmmf.{EmailAddress, String50, ZipCode}

String50.create("ilya.zinkovich@github.io") match {
  case Right(String50(string)) => EmailAddress.create(string)
  case Left(error) => Left(error)
}

ZipCode.create("12345")
