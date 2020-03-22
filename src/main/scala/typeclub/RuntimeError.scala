package typeclub

/** Errors that could be raised by the interpreter. */
sealed abstract class RuntimeError extends Product with Serializable

object RuntimeError {
  final case class SyntaxError(sexp: Expr)                    extends RuntimeError
  final case class Undefined(name: String, env: EnvF[Value])  extends RuntimeError
  final case class TypeError(actual: Value, expected: String) extends RuntimeError
}
