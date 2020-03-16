package typeclub

import fastparse.NoWhitespace._
import fastparse._

/** S-expression parser. */
object Parser {
  object parsers extends AllParsers

  def unsafeParse(code: String): Expr =
    parse(code) match {
      case Right(sexp) => sexp
      case Left(error) => throw new Exception(error.toString)
    }

  def parse(code: String): Either[ParseError, Expr] =
    parseWith(code, parsers.sexpToEnd(_))

  private def parseWith[A](code: String, parser: P[_] => P[A]): Either[ParseError, A] =
    fastparse.parse(code, parser) match {
      case failure: Parsed.Failure =>
        Left(ParseError(failure.msg))

      case Parsed.Success(value, _) =>
        Right(value)
    }
}

trait AllParsers {
  import Expr._

  def ws1[_: P]: P[Unit] =
    P(CharPred(_.isWhitespace))

  def anyParen[_: P]: P[Unit] =
    P(CharIn("()[]{}"))

  def identBreak[_: P]: P[Unit] =
    P(ws1 | anyParen)

  // Whitespace

  def ws[_: P]: P[Unit] =
    P(ws1.rep)

  // Atoms

  def num[_: P]: P[Expr] = {
    def sign: P[Unit] =
      P("+" | "-")

    def digits: P[Unit] =
      P(CharsWhileIn("0-9"))

    def exponent: P[Unit] =
      P(CharIn("Ee") ~ CharIn("+\\-").? ~ digits)

    P(
      sign.? ~ (
        (digits ~ "." ~ digits ~ exponent.?) |
          (digits ~ exponent) |
          (digits ~ ".") |
          ("." ~ digits ~ exponent.?) |
          digits
      )
    ).!.map(BigDecimal(_)).map(Num)
  }

  def str[_: P]: P[Expr] = {
    def escape: P[Unit] =
      P("\\" ~ AnyChar)

    def unescaped(quote: String): P[Unit] =
      P((!(quote | "\\") ~ AnyChar) | escape | "\\")

    def unescape(value: String): String =
      org.apache.commons.lang3.StringEscapeUtils.unescapeJava(value)

    def quoted(quote: String) =
      P(quote ~ (escape | unescaped(quote)).rep.!.map(unescape) ~ quote)

    def doubleQuoted: P[String] =
      quoted("\'")

    def singleQuoted: P[String] =
      quoted("\"")

    P(doubleQuoted | singleQuoted).map(Str)
  }

  def bool[_: P]: P[Expr] =
    P(("true" | "false").! ~ &(ws1 | anyParen | End)).map {
      case "true"  => Bool(true)
      case "false" => Bool(false)
    }

  def sym[_: P]: P[Expr] =
    P(CharsWhile(c => !c.isWhitespace && !"()[]{}".contains(c)).!).map(Sym)

  def atom[_: P]: P[Expr] =
    P(num | str | bool | sym)

  // Compound:

  def paren[_: P]: P[Expr] = {
    def parenWith(open: String, close: String): P[Expr] =
      P("(" ~ ws ~ expr.rep(sep = ws).map(_.toList).map(Paren) ~ ws ~ ")")

    P(parenWith("(", ")") | parenWith("[", "]") | parenWith("{", "}"))
  }

  // Complete expressions:

  def expr[_: P]: P[Expr] =
    P(atom | paren)

  def sexpToEnd[_: P]: P[Expr] =
    P(ws ~ expr ~ ws ~ End)
}
