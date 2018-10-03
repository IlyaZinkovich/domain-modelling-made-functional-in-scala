package io.github.ilyazinovich.dmmf

import cats.data.NonEmptyList
import cats.data.Validated.Invalid
import io.github.ilyazinovich.dmmf.ProductCode.CheckProductCodeExist
import org.scalatest.{FlatSpec, Matchers}

class AppTest extends FlatSpec with Matchers {

  "PlaceOrder" should "invalidate order lines" in {
    val line = UnvalidatedOrderLine("1", "W123", -10.0)
    val lines = List(line)
    val checkProductCodeExist: CheckProductCodeExist = (_: ProductCode) => true
    assertResult(Invalid(NonEmptyList.of(Error("Input is not a 5 chars string starting with W"),
      Error("Unable to validate quantity because of invalid product code")))) {
      ValidateOrder.validateOrderLines(lines, checkProductCodeExist)
    }
  }
}
