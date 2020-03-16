package typeclub

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ParserSpec extends AnyFreeSpec with Matchers {
  import Expr._
  import syntax._

  "boolean" in {
    expr"true" shouldBe Bool(true)
    expr"false" shouldBe Bool(false)
  }

  "number" in {
    expr"123" shouldBe Num(123)
    expr"123.456" shouldBe Num(123.456)
    expr".456" shouldBe Num(.456)
    expr"123E4" shouldBe Num(1230000)
    expr"123.456E4" shouldBe Num(1234560)
  }

  "string" in {
    expr"'hello'" shouldBe Str("hello")
    expr""""hello"""" shouldBe Str("hello")
    expr"'123'" shouldBe Str("123")
    expr""""true"""" shouldBe Str("true")
    expr""" "His name is 'Taco'" """ shouldBe Str("""His name is 'Taco'""")
    expr""" 'His name is "Taco"' """ shouldBe Str("""His name is "Taco"""")
    expr""" "His name is \"Taco\"" """ shouldBe Str("""His name is "Taco"""")
    expr""" 'His name is \'Taco\'' """ shouldBe Str("""His name is 'Taco'""")
  }

  "symbol" in {
    expr"hello" shouldBe Sym("hello")
    expr"true1" shouldBe Sym("true1")
    expr"false1" shouldBe Sym("false1")
  }

  "expr" in {
    expr"(+ 1 2)" shouldBe Paren(List(Sym("+"), Num(1), Num(2)))
    expr"(def (square x) (* x x))" shouldBe Paren(
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
