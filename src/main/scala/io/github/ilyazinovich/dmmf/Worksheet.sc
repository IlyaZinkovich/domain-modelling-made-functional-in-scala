import io.github.ilyazinovich.dmmf.ProductCode.CheckProductCodeExist
import io.github.ilyazinovich.dmmf._

val line = UnvalidatedOrderLine("1", "W123", -10.0)
val lines = List(line)
val checkProductCodeExist: CheckProductCodeExist = (_: ProductCode) => true
PlaceOrder.validateOrderLines(lines, checkProductCodeExist)