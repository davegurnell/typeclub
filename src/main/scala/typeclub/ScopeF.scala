package typeclub

/** A scope with in an environment. See also type aliases in the package object.
  * The type parameter should allow re-use for variables and types.
  */
final case class ScopeF[V](var bindings: Map[String, V] = Map.empty[String, V]) {
  def get(id: String): Option[V] =
    bindings.collectFirst { case (`id`, value) => value }

  def set(id: String, value: V): ScopeF[V] =
    ScopeF(bindings + ((id, value)))

  def destructiveSet(id: String, value: V): Unit =
    bindings = bindings + ((id, value))

  def destructiveSetAll(bindings: Seq[(String, V)]): Unit =
    bindings.foreach { case (id, value) => destructiveSet(id, value) }
}
