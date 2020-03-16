package typeclub

/** S-expression. Originally named SExp but that's curiously hard to type. */
sealed abstract class Expr extends Product with Serializable

object Expr {
  final case class Bool(value: Boolean)     extends Expr
  final case class Num(value: BigDecimal)   extends Expr
  final case class Str(value: String)       extends Expr
  final case class Sym(name: String)        extends Expr
  final case class Paren(items: List[Expr]) extends Expr
}
