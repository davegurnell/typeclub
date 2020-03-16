package typeclub

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class InterpreterSpec extends AnyFreeSpec with Matchers {
  import Interpreter.eval
  import syntax._

  "if" in {
    eval(expr"(if true 1 2)") shouldBe Right(Value.Num(1))
    eval(expr"(if false 1 2)") shouldBe Right(Value.Num(2))
    eval(expr"(if 1 2 3)") shouldBe Left(RuntimeError.TypeError(Value.Num(1), "boolean"))
  }

  "let" in {
    eval(expr"(let (a 1) a)") shouldBe Right(Value.Num(1))
    eval(expr"(let (a true) (if a 1 2))") shouldBe Right(Value.Num(1))
  }

  "unary" in {
    eval(expr"(neg 1)") shouldBe Right(Value.Num(-1))
    eval(expr"(not true)") shouldBe Right(Value.Bool(false))
  }

  "binary" in {
    eval(expr"(+ 3 2)") shouldBe Right(Value.Num(5))
    eval(expr"(- 3 2)") shouldBe Right(Value.Num(1))
    eval(expr"(* 3 2)") shouldBe Right(Value.Num(6))
    eval(expr"(/ 3 2)") shouldBe Right(Value.Num(1.5))
    eval(expr"(> 3 2)") shouldBe Right(Value.Bool(true))
    eval(expr"(> 3 3)") shouldBe Right(Value.Bool(false))
    eval(expr"(< 3 2)") shouldBe Right(Value.Bool(false))
    eval(expr"(< 3 3)") shouldBe Right(Value.Bool(false))
    eval(expr"(>= 3 2)") shouldBe Right(Value.Bool(true))
    eval(expr"(>= 3 3)") shouldBe Right(Value.Bool(true))
    eval(expr"(<= 3 2)") shouldBe Right(Value.Bool(false))
    eval(expr"(<= 3 3)") shouldBe Right(Value.Bool(true))
    eval(expr"(and true false)") shouldBe Right(Value.Bool(false))
    eval(expr"(and true true)") shouldBe Right(Value.Bool(true))
    eval(expr"(or true false)") shouldBe Right(Value.Bool(true))
    eval(expr"(or false false)") shouldBe Right(Value.Bool(false))
  }
}
