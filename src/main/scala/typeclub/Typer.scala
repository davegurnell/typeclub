package typeclub

/** An interpreter that checkuates s-expressions within an environment of variable bindings. */
object Typer {
  def typecheck(sexp: Expr, env: EnvF[Type] = EnvF()): Either[TypeError, Type] =
    // We push a stack frame on the env before we start
    // to prevent any destructive updates leaking out of the interpreter:
    TyperInternal.typecheck(sexp, env.push)
}

object TyperInternal {
  import Expr._
  import ExprMatchers._

  type Env = EnvF[Type]

  def typecheck(sexp: Expr, env: Env): Either[TypeError, Type] = {
    // println("check\n  " + sexp + "\n  " + env)
    sexp match {
      // Atomic expressions:
      case Bool(_)   => Right(Type.Bool)
      case Num(_)    => Right(Type.Num)
      case Str(_)    => Right(Type.Str)
      case Sym(name) => typecheckRef(name, env)

      // Built-in expressions:
      case App3("if", a, b, c)        => typecheckIf(a, b, c, env)
      case App2("let", App1(a, b), c) => typecheckLet(a, b, c, env)

      // Application expressions (could, in principle, be extended):
      case App1(op, a)    => typecheckUnary(op, a, env)
      case App2(op, a, b) => typecheckBinary(op, a, b, env)

      // Syntax errors:
      case sexp => Left(TypeError.SyntaxError(sexp))
    }
  }

  def typecheckIf(test: Expr, trueArm: Expr, falseArm: Expr, env: Env): Either[TypeError, Type] =
    for {
      _  <- typecheck(test, env).flatMap(assertEqual(_, Type.Bool))
      at <- typecheck(trueArm, env)
      bt <- typecheck(falseArm, env)
      rt <- lub(bt, at)
    } yield rt

  def typecheckLet(name: String, as: Expr, in: Expr, env: Env): Either[TypeError, Type] =
    for {
      tpe <- typecheck(as, env)
      ans <- typecheck(in, env.set(name, tpe))
    } yield ans

  def typecheckRef(name: String, env: Env): Either[TypeError, Type] =
    env.get(name).toRight(TypeError.Undefined(name, env))

  def typecheckUnary(op: String, a: Expr, env: Env): Either[TypeError, Type] = {
    def tc(at: Type, rt: Type)(a: Expr, env: Env): Either[TypeError, Type] =
      for {
        a <- typecheck(a, env).flatMap(assertEqual(_, at))
      } yield rt

    op match {
      case "neg"   => tc(Type.Num, Type.Num)(a, env)
      case "not"   => tc(Type.Bool, Type.Bool)(a, env)
      case "upper" => tc(Type.Str, Type.Str)(a, env)
      case "lower" => tc(Type.Str, Type.Str)(a, env)
    }
  }

  def typecheckBinary(op: String, a: Expr, b: Expr, env: Env): Either[TypeError, Type] = {
    def tc(at: Type, bt: Type, rt: Type)(a: Expr, b: Expr, env: Env): Either[TypeError, Type] =
      for {
        a <- typecheck(a, env).flatMap(assertEqual(_, at))
        a <- typecheck(b, env).flatMap(assertEqual(_, bt))
      } yield rt

    op match {
      case "+"      => tc(Type.Num, Type.Num, Type.Num)(a, b, env)
      case "-"      => tc(Type.Num, Type.Num, Type.Num)(a, b, env)
      case "*"      => tc(Type.Num, Type.Num, Type.Num)(a, b, env)
      case "/"      => tc(Type.Num, Type.Num, Type.Num)(a, b, env)
      case "="      => tc(Type.Num, Type.Num, Type.Bool)(a, b, env)
      case "!="     => tc(Type.Num, Type.Num, Type.Bool)(a, b, env)
      case ">"      => tc(Type.Num, Type.Num, Type.Bool)(a, b, env)
      case "<"      => tc(Type.Num, Type.Num, Type.Bool)(a, b, env)
      case ">="     => tc(Type.Num, Type.Num, Type.Bool)(a, b, env)
      case "<="     => tc(Type.Num, Type.Num, Type.Bool)(a, b, env)
      case "and"    => tc(Type.Bool, Type.Bool, Type.Bool)(a, b, env)
      case "or"     => tc(Type.Bool, Type.Bool, Type.Bool)(a, b, env)
      case "concat" => tc(Type.Str, Type.Str, Type.Str)(a, b, env)
    }
  }

  def assertEqual(actual: Type, expected: Type): Either[TypeError, Unit] =
    if (actual == expected) Right(()) else Left(TypeError.Mismatch(actual, expected))

  def lub(a: Type, b: Type): Either[TypeError, Type] =
    assertEqual(a, b).map(_ => a)
}
