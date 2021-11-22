package frawa.typedjson.schema

case class SchemaQuality(errors: Seq[SchemaError], ignoredKeywords: Set[String], pointer: Pointer = Pointer.empty) {
  def addIgnoredKeywords(ignoredKeywords: Set[String]): SchemaQuality =
    copy(ignoredKeywords = this.ignoredKeywords ++ ignoredKeywords)

  def addErrors(errors: Seq[SchemaError]): SchemaQuality =
    copy(errors = this.errors ++ errors)

  def combine(other: SchemaQuality): SchemaQuality =
    copy(errors = this.errors ++ other.errors, ignoredKeywords = this.ignoredKeywords ++ other.ignoredKeywords)
}

object SchemaQuality {
  val empty: SchemaQuality = SchemaQuality(Seq.empty, Set.empty)
}
