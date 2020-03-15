package typeclub

sealed abstract class RuntimeError extends Product with Serializable

object RuntimeError {
  final case class Undefined(name: String, env: Env)             extends RuntimeError
  final case class SyntaxError(sexp: Expr)                       extends RuntimeError
  final case class DecoderError(actual: Value, expected: String) extends RuntimeError
}
