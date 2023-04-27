package frawa.typedjson.eval

import frawa.typedjson.keywords.WithPointer
import frawa.typedjson.validation.ValidationError
import frawa.typedjson.validation.ValidationAnnotation
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.keywords.SchemaProblems
import frawa.typedjson.validation.NotOneOf
import frawa.typedjson.validation.NotInvalid
import frawa.typedjson.keywords.Evaluated

// TODO this will converge to "basic" output format,
// see https://json-schema.org/draft/2020-12/json-schema-core.html#name-basic

case class BasicOutput(
    valid: Boolean,
    errors: Seq[BasicOutput.Error] = Seq(),
    pointer: Pointer = Pointer.empty,
    annotations: Seq[Evaluated] = Seq.empty
)

object BasicOutput:
  type Error = WithPointer[ValidationError]

  given OutputOps[BasicOutput] with
    def valid(pointer: Pointer): BasicOutput = BasicOutput(true, Seq(), pointer)
    def invalid(error: ValidationError, pointer: Pointer): BasicOutput =
      BasicOutput(false, Seq(WithPointer(error, pointer)), pointer)

    def all(os: Seq[BasicOutput], pointer: Pointer): BasicOutput =
      BasicOutput(
        os.forall(_.valid),
        os.flatMap(_.errors),
        pointer,
        OutputOps.mergeAnnotations(os.filter(_.pointer == pointer).flatMap(_.annotations))
      )

    extension (o: BasicOutput)
      def not(pointer: Pointer): BasicOutput =
        if o.valid then o.copy(valid = false, errors = Seq(WithPointer(NotInvalid(), pointer)))
        else o.copy(valid = true, errors = Seq())
      def isValid: Boolean                                   = o.valid
      def withAnnotation(annotation: Evaluated): BasicOutput = o.copy(annotations = o.annotations :+ annotation)
      def getAnnotations(): Seq[Evaluated]                   = o.annotations
