package typeclub.sexp

import syntax._
import minitest._

object ParserSuite extends SimpleTestSuite {
  import SExp._

  test("boolean") {
    assertEquals(
      sexp"true",
      Bool(true)
    )
    assertEquals(
      sexp"false",
      Bool(false)
    )
  }

  test("number") {
    assertEquals(
      sexp"123",
      Num(123)
    )
    assertEquals(
      sexp"123.456",
      Num(123.456)
    )
    assertEquals(
      sexp".456",
      Num(.456)
    )
    assertEquals(
      sexp"123E4",
      Num(1230000)
    )
    assertEquals(
      sexp"123.456E4",
      Num(1234560)
    )
  }

  test("string") {
    assertEquals(
      sexp"'hello'",
      Str("hello")
    )
    assertEquals(
      sexp""""hello"""",
      Str("hello")
    )
    assertEquals(
      sexp"'123'",
      Str("123")
    )
    assertEquals(
      sexp""""true"""",
      Str("true")
    )
    assertEquals(
      sexp""" "His name is 'Taco'" """,
      Str("""His name is 'Taco'""")
    )
    assertEquals(
      sexp""" 'His name is "Taco"' """,
      Str("""His name is "Taco"""")
    )
    assertEquals(
      sexp""" "His name is \"Taco\"" """,
      Str("""His name is "Taco"""")
    )
    assertEquals(
      sexp""" 'His name is \'Taco\'' """,
      Str("""His name is 'Taco'""")
    )
  }

  test("symbol") {
    assertEquals(
      sexp"hello",
      Sym("hello")
    )
    assertEquals(
      sexp"true1",
      Sym("true1")
    )
    assertEquals(
      sexp"false1",
      Sym("false1")
    )
  }

  test("expr") {
    assertEquals(
      sexp"(+ 1 2)",
      Expr(List(Sym("+"), Num(1), Num(2)))
    )
    assertEquals(
      sexp"(def (square x) (* x x))",
      Expr(
        List(
          Sym("def"),
          Expr(
            List(
              Sym("square"),
              Sym("x")
            )
          ),
          Expr(
            List(
              Sym("*"),
              Sym("x"),
              Sym("x")
            )
          )
        )
      )
    )
  }
}
