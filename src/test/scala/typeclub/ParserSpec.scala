package typeclub

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ParserSpec extends AnyFreeSpec with Matchers {
  import Expr._
  import syntax._

  "boolean" in {
    sexp"true" shouldBe Bool(true)
    sexp"false" shouldBe Bool(false)
  }

  "number" in {
    sexp"123" shouldBe Num(123)
    sexp"123.456" shouldBe Num(123.456)
    sexp".456" shouldBe Num(.456)
    sexp"123E4" shouldBe Num(1230000)
    sexp"123.456E4" shouldBe Num(1234560)
  }

  "string" in {
    sexp"'hello'" shouldBe Str("hello")
    sexp""""hello"""" shouldBe Str("hello")
    sexp"'123'" shouldBe Str("123")
    sexp""""true"""" shouldBe Str("true")
    sexp""" "His name is 'Taco'" """ shouldBe Str("""His name is 'Taco'""")
    sexp""" 'His name is "Taco"' """ shouldBe Str("""His name is "Taco"""")
    sexp""" "His name is \"Taco\"" """ shouldBe Str("""His name is "Taco"""")
    sexp""" 'His name is \'Taco\'' """ shouldBe Str("""His name is 'Taco'""")
  }

  "symbol" in {
    sexp"hello" shouldBe Sym("hello")
    sexp"true1" shouldBe Sym("true1")
    sexp"false1" shouldBe Sym("false1")
  }

  "expr" in {
    sexp"(+ 1 2)" shouldBe Paren(List(Sym("+"), Num(1), Num(2)))
    sexp"(def (square x) (* x x))" shouldBe Paren(
      List(
        Sym("def"),
        Paren(
          List(
            Sym("square"),
            Sym("x")
          )
        ),
        Paren(
          List(
            Sym("*"),
            Sym("x"),
            Sym("x")
          )
        )
      )
    )
  }
}
