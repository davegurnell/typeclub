package typeclub.sexp

import minitest._

object ParserSuite extends SimpleTestSuite {
  import SExp._

  test("boolean") {
    assertEquals(
      Parser("true"),
      Right(Bool(true))
    )
    assertEquals(
      Parser("false"),
      Right(Bool(false))
    )
  }

  test("number") {
    assertEquals(
      Parser("123"),
      Right(Num(123))
    )
    assertEquals(
      Parser("123.456"),
      Right(Num(123.456))
    )
    assertEquals(
      Parser(".456"),
      Right(Num(.456))
    )
    assertEquals(
      Parser("123E4"),
      Right(Num(1230000))
    )
    assertEquals(
      Parser("123.456E4"),
      Right(Num(1234560))
    )
  }

  test("string") {
    assertEquals(
      Parser("'hello'"),
      Right(Str("hello"))
    )
    assertEquals(
      Parser("\"hello\""),
      Right(Str("hello"))
    )
    assertEquals(
      Parser("'123'"),
      Right(Str("123"))
    )
    assertEquals(
      Parser("\"true\""),
      Right(Str("true"))
    )
    assertEquals(
      Parser(""" "His name is 'Taco'" """),
      Right(Str("""His name is 'Taco'"""))
    )
    assertEquals(
      Parser(""" 'His name is "Taco"' """),
      Right(Str("""His name is "Taco""""))
    )
    assertEquals(
      Parser(""" "His name is \"Taco\"" """),
      Right(Str("""His name is "Taco""""))
    )
    assertEquals(
      Parser(""" 'His name is \'Taco\'' """),
      Right(Str("""His name is 'Taco'"""))
    )
  }

  test("symbol") {
    assertEquals(
      Parser("hello"),
      Right(Sym("hello"))
    )
    assertEquals(
      Parser("true1"),
      Right(Sym("true1"))
    )
    assertEquals(
      Parser("false1"),
      Right(Sym("false1"))
    )
  }

  test("expr") {
    assertEquals(
      Parser("(+ 1 2)"),
      Right(Expr(List(Sym("+"), Num(1), Num(2))))
    )
    assertEquals(
      Parser("(def (square x) (* x x))"),
      Right(
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
    )
  }
}
