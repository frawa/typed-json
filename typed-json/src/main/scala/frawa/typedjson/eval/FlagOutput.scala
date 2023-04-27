package frawa.typedjson.eval

import frawa.typedjson.validation.ValidationAnnotation
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.validation.ValidationError
import frawa.typedjson.keywords.SchemaProblems
import frawa.typedjson.keywords.Evaluated
import frawa.typedjson.keywords.Keyword

// see https://json-schema.org/draft/2020-12/json-schema-core.html#name-flag

case class FlagOutput(valid: Boolean, pointer: Pointer = Pointer.empty, annotations: Seq[Evaluated] = Seq.empty)

object FlagOutput:
  given OutputOps[FlagOutput] with
    def valid(pointer: Pointer): FlagOutput                           = FlagOutput(true, pointer)
    def invalid(error: ValidationError, pointer: Pointer): FlagOutput = FlagOutput(false, pointer)

    def all(os: Seq[FlagOutput], pointer: Pointer): FlagOutput =
      FlagOutput(
        os.forall(_.valid),
        pointer,
        OutputOps.mergeAnnotations(os.filter(_.pointer == pointer).flatMap(_.annotations))
      )

    extension (o: FlagOutput)
      def not(pointer: Pointer): FlagOutput                 = o.copy(valid = !o.valid)
      def isValid: Boolean                                  = o.valid
      def withAnnotation(annotation: Evaluated): FlagOutput = o.copy(annotations = o.annotations :+ annotation)
      def getAnnotations(): Seq[Evaluated]                  = o.annotations
      def forKeyword(k: Keyword): FlagOutput                = o
