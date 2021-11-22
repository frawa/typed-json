package frawa.typedjson.schema

object Checked {
  type Annotation = WithPointer[Observation2]

  def apply[R](valid: Boolean, result: R): Checked[R] = Checked[R](valid, Seq(result))
  def valid[R]: Checked[R]                            = Checked[R](true)
  def valid[R](result: R): Checked[R]                 = Checked[R](true, Seq(result))
  def invalid[R]: Checked[R]                          = Checked[R](false)
  def invalid[R](result: R): Checked[R]               = Checked[R](false, Seq(result))

  def merge[R](checked: Seq[Checked[R]]): Checked[R] = {
    val valid           = checked.forall(_.valid)
    val results: Seq[R] = checked.flatMap(_.results)
    val validation      = checked.map(_.validation).reduceOption(_.combine(_)).getOrElse(SchemaQuality.empty)
    val annotations     = checked.filter(_.valid).flatMap(_.annotations)
    Checked(valid, results, annotations, validation, 1 + count(checked))
  }
  def count[R](checked: Seq[Checked[R]]): Int = checked.map(_.count).sum
}

case class Checked[R](
    valid: Boolean,
    results: Seq[R] = Seq.empty,
    annotations: Seq[Checked.Annotation] = Seq.empty,
    validation: SchemaQuality = SchemaQuality.empty,
    count: Int = 1
) {
  def add(others: Seq[Checked[R]]): Checked[R] =
    this
      .copy(count = this.count + Checked.count(others))
      .addAnnotations(others.flatMap(_.annotations))
      .addValidations(others.map(_.validation))
  def add(validation: SchemaQuality): Checked[R] = this.copy(validation = this.validation.combine(validation))
  private def addValidations(validations: Seq[SchemaQuality]): Checked[R] =
    this.copy(validation = validations.foldLeft(this.validation)(_.combine(_)))

  def add(annotation: Checked.Annotation): Checked[R] = this.copy(annotations = this.annotations :+ annotation)
  private def addAnnotations(annotations: Seq[Checked.Annotation]) =
    this.copy(annotations = this.annotations ++ annotations)
}
