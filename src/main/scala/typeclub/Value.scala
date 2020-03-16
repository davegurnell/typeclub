package typeclub

/** Runtime value returned by evaluating an s-expression. */
sealed abstract class Value extends Product with Serializable

object Value {
  final case class Str(value: String)     extends Value
  final case class Num(value: BigDecimal) extends Value
  final case class Bool(value: Boolean)   extends Value
}
