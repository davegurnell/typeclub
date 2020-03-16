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
      case value         => Left(RuntimeError.DecoderError(value, "boolean"))
    }

  implicit def intDecoder: ValueDecoder[Int] =
    instance {
      case Value.Num(v) => Right(v.toInt)
      case value        => Left(RuntimeError.DecoderError(value, "integer"))
    }

  implicit def stringDecoder: ValueDecoder[String] =
    instance {
      case Value.Str(v) => Right(v)
      case value        => Left(RuntimeError.DecoderError(value, "string"))
    }
}
