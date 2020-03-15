package typeclub

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
