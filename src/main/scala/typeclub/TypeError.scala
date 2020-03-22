package typeclub

/** Errors that could be raised by the typer. */
sealed abstract class TypeError extends Product with Serializable

object TypeError {
  final case class SyntaxError(sexp: Expr)                  extends TypeError
  final case class Undefined(name: String, env: EnvF[Type]) extends TypeError
  final case class Mismatch(actual: Type, expected: Type)   extends TypeError
}
