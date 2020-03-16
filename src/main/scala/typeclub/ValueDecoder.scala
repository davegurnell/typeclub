package typeclub

/** Type class for converting instances of `Value` to Scala values. */
trait ValueDecoder[A] {
  def decode(value: Value): Either[RuntimeError, A]
}

object ValueDecoder {
  def apply[A](implicit decoder: ValueDecoder[A]): ValueDecoder[A] =
    decoder

  def instance[A](func: Value => Either[RuntimeError, A]): ValueDecoder[A] =
    (value: Value) => func(value)

  implicit def booleanDecoder: ValueDecoder[Boolean] =
    instance {
      case Value.Bool(v) => Right(v)
      case value         => Left(RuntimeError.TypeError(value, "boolean"))
    }

  implicit def bigDecimalDecoder: ValueDecoder[BigDecimal] =
    instance {
      case Value.Num(v) => Right(v.toInt)
      case value        => Left(RuntimeError.TypeError(value, "number"))
    }

  implicit def stringDecoder: ValueDecoder[String] =
    instance {
      case Value.Str(v) => Right(v)
      case value        => Left(RuntimeError.TypeError(value, "string"))
    }
}
