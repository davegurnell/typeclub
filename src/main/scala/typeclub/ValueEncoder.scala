package typeclub

/** Type class for converting Scala values to instances of `Value`. */
trait ValueEncoder[A] {
  def encode(value: A): Value
}

object ValueEncoder {
  def apply[A](implicit encoder: ValueEncoder[A]): ValueEncoder[A] =
    encoder

  def instance[A](func: A => Value): ValueEncoder[A] =
    (a: A) => func(a)

  implicit def booleanEncoder: ValueEncoder[Boolean] =
    instance(Value.Bool)

  implicit def bigDecimalEncoder: ValueEncoder[BigDecimal] =
    instance(Value.Num)

  implicit def stringEncoder: ValueEncoder[String] =
    instance(Value.Str)
}
