package typeclub

/** An "execution environment" containing variable bindings.
  * The type parameter should allow re-use for variables and types.
  * Essentially a chain of nested scopes.
  * See also type aliases in the package object.
  */
final case class EnvF[V](scopes: List[ScopeF[V]] = Nil) {
  def get(id: String): Option[V] = {
    def loop(scopes: List[ScopeF[V]]): Option[V] =
      scopes match {
        case head :: tail => head.get(id).orElse(loop(tail))
        case Nil          => None
      }

    loop(scopes)
  }

  def set(id: String, value: V): EnvF[V] =
    EnvF(scopes.head.set(id, value) :: scopes.tail)

  def destructiveSet(id: String, value: V): Unit =
    scopes.head.destructiveSet(id, value)

  def destructiveSetAll(bindings: Seq[(String, V)]): Unit =
    scopes.head.destructiveSetAll(bindings)

  def push: EnvF[V] =
    EnvF(ScopeF[V]() :: scopes)

  def pop: EnvF[V] =
    EnvF(scopes.tail)
}
