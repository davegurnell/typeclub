package typeclub

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
  import SExpMatchers._

  def evalAs[A: ValueDecoder](sexp: Expr, env: Env): Either[RuntimeError, A] =
    eval(sexp, env).flatMap(ValueDecoder[A].decode)

  def eval(sexp: Expr, env: Env): Either[RuntimeError, Value] = {
    // println("eval\n  " + sexp + "\n  " + env)
    sexp match {
      case Bool(value)                => Right(Value.Bool(value))
      case Num(value)                 => Right(Value.Num(value))
      case Str(value)                 => Right(Value.Str(value))
      case Sym(name)                  => evalRef(name, env)
      case App3("if", a, b, c)        => evalIf(a, b, c, env)
      case App2("let", App1(a, b), c) => evalLet(a, b, c, env)
      case sexp                       => Left(RuntimeError.SyntaxError(sexp))
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
}
