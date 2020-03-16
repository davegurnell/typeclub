package typeclub

object syntax {

  /** String interpolator syntax for s-expressions. Enables code like:
    *
    * ```scala
    * expr"(+ 1 1)"
    * ```
    */
  implicit class ParserOps(val sc: StringContext) extends AnyVal {
    def expr(args: Any*): Expr =
      Parser.unsafeParse(code(sc, args))

    def expre(args: Any*): Either[ParseError, Expr] =
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
