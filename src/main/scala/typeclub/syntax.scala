package typeclub

object syntax {
  implicit class SExpStringOps(val sc: StringContext) extends AnyVal {
    def sexp(args: Any*): Expr =
      Parser.unsafeParse(code(sc, args))

    def sexpe(args: Any*): Either[ParseError, Expr] =
      Parser.parse(code(sc, args))

    // This is very dumb. We just toString all the args and
    // inline them in the rest of the string:
    private def code(sc: StringContext, args: Seq[Any]): String = {
      sc.parts
        .zip((args :+ "").map(_.toString))
        .map { case (a, b) => a + b }
        .mkString
    }
  }
}
