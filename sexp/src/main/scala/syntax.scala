package typeclub.sexp

object syntax {
  implicit class SExpStringOps(val sc: StringContext) extends AnyVal {
    def sexp(): SExp =
      Parser.unsafeParse(sc.parts.mkString)

    def sexpe(): Either[ParseError, SExp] =
      Parser.parse(sc.parts.mkString)
  }
}
