package object typeclub {

  /** Environment used at runtime by the interpreter. */
  type Env = EnvF[Value]

  /** Scope used at runtime by the interpreter. */
  type Scope = ScopeF[Value]

}
