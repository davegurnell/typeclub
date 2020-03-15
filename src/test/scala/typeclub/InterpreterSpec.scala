package typeclub

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class InterpreterSpec extends AnyFreeSpec with Matchers {
  import Interpreter.eval
  import syntax._

  "if" in {
    eval(sexp"(if true 1 2)") shouldBe Right(Value.Num(1))
    eval(sexp"(if false 1 2)") shouldBe Right(Value.Num(2))
    eval(sexp"(if 1 2 3)") shouldBe Left(RuntimeError.DecoderError(Value.Num(1), "boolean"))
  }

  "let" in {
    eval(sexp"(let (a 1) a)") shouldBe Right(Value.Num(1))
    eval(sexp"(let (a true) (if a 1 2))") shouldBe Right(Value.Num(1))
  }
}
