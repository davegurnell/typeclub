package typeclub

/** An interpreter that evaluates s-expressions within an environment of variable bindings. */
object Interpreter {
  def evalAs[A: ValueDecoder](sexp: Expr, env: Env = EnvF()): Either[RuntimeError, A] =
    // We push a stack frame on the env before we start
    // to prevent any destructive updates leaking out of the interpreter:
    InterpreterInternal.evalAs(sexp, env.push)

  def eval(sexp: Expr, env: Env = EnvF()): Either[RuntimeError, Value] =
    // We push a stack frame on the env before we start
    // to prevent any destructive updates leaking out of the interpreter:
    InterpreterInternal.eval(sexp, env.push)
}

object InterpreterInternal {
  import Expr._
  import ExprMatchers._

  def evalAs[A: ValueDecoder](sexp: Expr, env: Env): Either[RuntimeError, A] =
    eval(sexp, env).flatMap(ValueDecoder[A].decode)

  def eval(sexp: Expr, env: Env): Either[RuntimeError, Value] = {
    // println("eval\n  " + sexp + "\n  " + env)
    sexp match {
      // Atomic expressions:
      case Bool(value) => Right(Value.Bool(value))
      case Num(value)  => Right(Value.Num(value))
      case Str(value)  => Right(Value.Str(value))
      case Sym(name)   => evalRef(name, env)

      // Built-in expressions:
      case App3("if", a, b, c)        => evalIf(a, b, c, env)
      case App2("let", App1(a, b), c) => evalLet(a, b, c, env)

      // Application expressions (could, in principle, be extended):
      case App1(op, a)    => evalUnary(op, a, env)
      case App2(op, a, b) => evalBinary(op, a, b, env)

      // Syntax errors:
      case sexp => Left(RuntimeError.SyntaxError(sexp))
    }
  }

  def evalIf(test: Expr, trueArm: Expr, falseArm: Expr, env: Env): Either[RuntimeError, Value] =
    for {
      test   <- evalAs[Boolean](test, env)
      result <- if (test) eval(trueArm, env) else eval(falseArm, env)
    } yield result

  def evalLet(name: String, as: Expr, in: Expr, env: Env): Either[RuntimeError, Value] =
    for {
      value <- eval(as, env)
      ans   <- eval(in, env.set(name, value))
    } yield ans

  def evalRef(name: String, env: Env): Either[RuntimeError, Value] =
    env.get(name).toRight(RuntimeError.Undefined(name, env))

  def evalUnary(op: String, a: Expr, env: Env): Either[RuntimeError, Value] = {
    def evalTyped[A: ValueDecoder, R: ValueEncoder](a: Expr, env: Env)(func: A => R): Either[RuntimeError, Value] =
      for {
        a <- evalAs[A](a, env)
        b = ValueEncoder[R].encode(func(a))
      } yield b

    op match {
      case "neg"   => evalTyped[BigDecimal, BigDecimal](a, env)(-_)
      case "not"   => evalTyped[Boolean, Boolean](a, env)(!_)
      case "upper" => evalTyped[String, String](a, env)(_.toUpperCase)
      case "lower" => evalTyped[String, String](a, env)(_.toLowerCase)
    }
  }

  def evalBinary(op: String, a: Expr, b: Expr, env: Env): Either[RuntimeError, Value] = {
    def evalTyped[A: ValueDecoder, B: ValueDecoder, R: ValueEncoder](a: Expr, b: Expr, env: Env)(func: (A, B) => R): Either[RuntimeError, Value] =
      for {
        a <- evalAs[A](a, env)
        b <- evalAs[B](b, env)
        r = ValueEncoder[R].encode(func(a, b))
      } yield r

    op match {
      case "+"      => evalTyped[BigDecimal, BigDecimal, BigDecimal](a, b, env)(_ + _)
      case "-"      => evalTyped[BigDecimal, BigDecimal, BigDecimal](a, b, env)(_ - _)
      case "*"      => evalTyped[BigDecimal, BigDecimal, BigDecimal](a, b, env)(_ * _)
      case "/"      => evalTyped[BigDecimal, BigDecimal, BigDecimal](a, b, env)(_ / _)
      case "="      => evalTyped[BigDecimal, BigDecimal, Boolean](a, b, env)(_ == _)
      case "!="     => evalTyped[BigDecimal, BigDecimal, Boolean](a, b, env)(_ != _)
      case ">"      => evalTyped[BigDecimal, BigDecimal, Boolean](a, b, env)(_ > _)
      case "<"      => evalTyped[BigDecimal, BigDecimal, Boolean](a, b, env)(_ < _)
      case ">="     => evalTyped[BigDecimal, BigDecimal, Boolean](a, b, env)(_ >= _)
      case "<="     => evalTyped[BigDecimal, BigDecimal, Boolean](a, b, env)(_ <= _)
      case "and"    => evalTyped[Boolean, Boolean, Boolean](a, b, env)(_ && _)
      case "or"     => evalTyped[Boolean, Boolean, Boolean](a, b, env)(_ || _)
      case "concat" => evalTyped[String, String, String](a, b, env)(_ + _)
    }
  }

}
