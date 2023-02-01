package frawa.typedjson.eval

import frawa.typedjson.keywords.WithPointer
import frawa.typedjson.validation.ValidationError
import frawa.typedjson.validation.ValidationAnnotation
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.keywords.SchemaProblems
import frawa.typedjson.validation.NotOneOf
import frawa.typedjson.validation.NotInvalid

// TODO this will converge to "basic" output format,
// see https://json-schema.org/draft/2020-12/json-schema-core.html#name-basic

case class BasicOutput(valid: Boolean, errors: Seq[BasicOutput.Error])

object BasicOutput {
  type Error = WithPointer[ValidationError]

  given OutputOps[BasicOutput] with
    def valid(pointer: Pointer): BasicOutput                                   = BasicOutput(true, Seq())
    def valid(annotation: ValidationAnnotation, pointer: Pointer): BasicOutput = BasicOutput(true, Seq())
    def invalid(error: ValidationError, pointer: Pointer): BasicOutput =
      BasicOutput(false, Seq(WithPointer(error, pointer)))
    def invalid(problems: SchemaProblems): BasicOutput = BasicOutput(false, Seq())

    def all(os: Seq[BasicOutput], pointer: Pointer): BasicOutput = BasicOutput(os.forall(_.valid), os.flatMap(_.errors))

    // def contains(os: Seq[BasicOutput], min: Option[Int], max: Option[Int], pointer: Pointer): BasicOutput = ???

    extension (o: BasicOutput)
      def not(pointer: Pointer): BasicOutput =
        if o.valid then o.copy(valid = false, errors = Seq(WithPointer(NotInvalid(), pointer)))
        else o.copy(valid = true, errors = Seq())
      def isValid: Boolean = o.valid
}
