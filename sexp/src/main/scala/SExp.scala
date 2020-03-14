package typeclub.sexp

sealed abstract class SExp extends Product with Serializable

object SExp {
  final case class Sym(name: String) extends SExp
  final case class Str(value: String) extends SExp
  final case class Num(value: BigDecimal) extends SExp
  final case class Bool(value: Boolean) extends SExp
  final case class Expr(items: List[SExp]) extends SExp
}
