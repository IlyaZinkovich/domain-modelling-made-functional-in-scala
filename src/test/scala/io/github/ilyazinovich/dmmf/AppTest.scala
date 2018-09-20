package io.github.ilyazinovich.dmmf

import org.scalatest.{FlatSpec, Matchers}

class AppTest extends FlatSpec with Matchers {

  "String50" should "return Right None for empty String" in {
    String50.create("") should be (Right(None))
  }

  "String50" should "return Left Error for non-empty String longer than 50 chars" in {
    String50.create("a" * 51) should be (Left(Error("String is longer than 50 characters")))
  }
}
