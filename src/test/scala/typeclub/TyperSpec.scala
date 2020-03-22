package typeclub

package typeclub

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TyperSpec extends AnyFreeSpec with Matchers {
  import Typer.typecheck
  import syntax._

  "if" in {
    typecheck(expr"(if true 1 2)") shouldBe Right(Type.Num)
    typecheck(expr"(if false 1 2)") shouldBe Right(Type.Num)

    typecheck(expr"(if 1 2 3)") shouldBe Left(TypeError.Mismatch(Type.Num, Type.Bool))
    typecheck(expr"(if true 2 '3')") shouldBe Left(TypeError.Mismatch(Type.Str, Type.Num))
  }

  "let" in {
    typecheck(expr"(let (a 1) a)") shouldBe Right(Type.Num)
    typecheck(expr"(let (a true) (if a 1 2))") shouldBe Right(Type.Num)
  }

  "unary" in {
    typecheck(expr"(neg 1)") shouldBe Right(Type.Num)
    typecheck(expr"(not true)") shouldBe Right(Type.Bool)
    typecheck(expr"(upper 'Hello')") shouldBe Right(Type.Str)
    typecheck(expr"(lower 'Hello')") shouldBe Right(Type.Str)

    typecheck(expr"(neg '1')") shouldBe Left(TypeError.Mismatch(Type.Str, Type.Num))
    typecheck(expr"(not 'true')") shouldBe Left(TypeError.Mismatch(Type.Str, Type.Bool))
    typecheck(expr"(upper 123)") shouldBe Left(TypeError.Mismatch(Type.Num, Type.Str))
    typecheck(expr"(lower true)") shouldBe Left(TypeError.Mismatch(Type.Bool, Type.Str))
  }

  "binary" in {
    typecheck(expr"(+ 3 2)") shouldBe Right(Type.Num)
    typecheck(expr"(- 3 2)") shouldBe Right(Type.Num)
    typecheck(expr"(* 3 2)") shouldBe Right(Type.Num)
    typecheck(expr"(/ 3 2)") shouldBe Right(Type.Num)
    typecheck(expr"(> 3 2)") shouldBe Right(Type.Bool)
    typecheck(expr"(< 3 2)") shouldBe Right(Type.Bool)
    typecheck(expr"(>= 3 2)") shouldBe Right(Type.Bool)
    typecheck(expr"(<= 3 2)") shouldBe Right(Type.Bool)
    typecheck(expr"(and true false)") shouldBe Right(Type.Bool)
    typecheck(expr"(or true false)") shouldBe Right(Type.Bool)
    typecheck(expr"(concat 'hello' 'world')") shouldBe Right(Type.Str)

    typecheck(expr"(+ 3 true)") shouldBe Left(TypeError.Mismatch(Type.Bool, Type.Num))
    typecheck(expr"(- 3 true)") shouldBe Left(TypeError.Mismatch(Type.Bool, Type.Num))
    typecheck(expr"(* 3 true)") shouldBe Left(TypeError.Mismatch(Type.Bool, Type.Num))
    typecheck(expr"(/ 3 true)") shouldBe Left(TypeError.Mismatch(Type.Bool, Type.Num))
    typecheck(expr"(> 3 true)") shouldBe Left(TypeError.Mismatch(Type.Bool, Type.Num))
    typecheck(expr"(< 3 true)") shouldBe Left(TypeError.Mismatch(Type.Bool, Type.Num))
    typecheck(expr"(>= 3 true)") shouldBe Left(TypeError.Mismatch(Type.Bool, Type.Num))
    typecheck(expr"(<= 3 true)") shouldBe Left(TypeError.Mismatch(Type.Bool, Type.Num))
    typecheck(expr"(and 3 true)") shouldBe Left(TypeError.Mismatch(Type.Num, Type.Bool))
    typecheck(expr"(or 3 true)") shouldBe Left(TypeError.Mismatch(Type.Num, Type.Bool))
    typecheck(expr"(concat 'hellp' true)") shouldBe Left(TypeError.Mismatch(Type.Bool, Type.Str))
  }
}
