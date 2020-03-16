package typeclub

/** Errors that could be raised while parsing an s-expression.
  * This is currently a bit lightweight. It could easily be improved in the future.
  */
final case class ParseError(message: String)
