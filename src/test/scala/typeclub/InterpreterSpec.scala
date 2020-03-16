package typeclub

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class InterpreterSpec extends AnyFreeSpec with Matchers {
  import Interpreter.eval
  import syntax._

  "if" in {
    eval(expr"(if true 1 2)") shouldBe Right(Value.Num(1))
    eval(expr"(if false 1 2)") shouldBe Right(Value.Num(2))
    eval(expr"(if 1 2 3)") shouldBe Left(RuntimeError.DecoderError(Value.Num(1), "boolean"))
  }

  "let" in {
    eval(expr"(let (a 1) a)") shouldBe Right(Value.Num(1))
    eval(expr"(let (a true) (if a 1 2))") shouldBe Right(Value.Num(1))
  }
}
