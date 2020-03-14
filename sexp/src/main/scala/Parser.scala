package typeclub.sexp

import fastparse._
import fastparse.NoWhitespace._

object Parser {
  object parsers extends AllParsers

  def apply(code: String): Either[ParseError, SExp] =
    parse(code, parsers.sexpToEnd(_))

  private def parse[A](code: String, parser: P[_] => P[A]): Either[ParseError, A] =
    fastparse.parse(code, parser) match {
      case failure: Parsed.Failure =>
        Left(ParseError(failure.msg))

      case Parsed.Success(value, _) =>
        Right(value)
    }
}

//noinspection ForwardReference,ScalaUnusedSymbol
trait AllParsers {
  import SExp._

  def whitespace[_: P]: P[Unit] =
    P(CharPred(_.isWhitespace))

  // def alpha[_: P]: P[Unit] =
  //   P(CharIn("_$", "a-z", "A-Z"))

  def digit[_: P]: P[Unit] =
    P(CharIn("0-9"))

  def paren[_: P]: P[Unit] =
    P(CharIn("()"))

  def identBreak[_: P]: P[Unit] =
    P(whitespace | paren)

  // Whitespace

  def ws[_: P]: P[Unit] =
    P(whitespace.rep)

  // Atoms

  def num[_: P]: P[SExp] = {
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

  def str[_: P]: P[SExp] = {
    def newline: P[Unit] =
      P("\n" | "\r\n" | "\r" | "\f")

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

  def bool[_: P]: P[SExp] =
    P(("true" | "false").! ~ &(whitespace | paren | End)).map {
      case "true"  => Bool(true)
      case "false" => Bool(false)
    }

  def sym[_: P]: P[SExp] =
    P(CharsWhile(c => !c.isWhitespace && c != '(' && c != ')').!).map(Sym(_))

  def atom[_: P]: P[SExp] =
    P(num | str | bool | sym)

  // Compound:

  def expr[_: P]: P[SExp] =
    P("(" ~ ws ~ sexp.rep(sep = ws).map(_.toList).map(Expr(_)) ~ ws ~ ")")

  // Complete SExps:

  def sexp[_: P]: P[SExp] =
    P(atom | expr)

  def sexpToEnd[_: P]: P[SExp] =
    P(ws ~ sexp ~ ws ~ End)
}
