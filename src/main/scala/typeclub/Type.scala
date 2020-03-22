package typeclub

/** Type of an s-expression. */
sealed abstract class Type extends Product with Serializable

object Type {
  final case object Str  extends Type
  final case object Num  extends Type
  final case object Bool extends Type
}
