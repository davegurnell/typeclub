package typeclub

object SExpMatchers {

  import Expr._

  object App1 {
    def unapply(sexp: Expr): Option[(String, Expr)] =
      sexp match {
        case Paren(List(Sym(name), a)) => Some((name, a))
        case _ => None
      }
  }

  object App2 {
    def unapply(sexp: Expr): Option[(String, Expr, Expr)] =
      sexp match {
        case Paren(List(Sym(name), a, b)) => Some((name, a, b))
        case _ => None
      }
  }

  object App3 {
    def unapply(sexp: Expr): Option[(String, Expr, Expr, Expr)] =
      sexp match {
        case Paren(List(Sym(name), a, b, c)) => Some((name, a, b, c))
        case _ => None
      }
  }

}
