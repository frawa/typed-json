package frawa.typedjson.eval

import frawa.typedjson.validation.ValidationAnnotation
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.validation.ValidationError
import frawa.typedjson.keywords.SchemaProblems
import frawa.typedjson.keywords.Evaluated

// see https://json-schema.org/draft/2020-12/json-schema-core.html#name-flag

case class FlagOutput(valid: Boolean, annotation: Option[Evaluated] = None)

object FlagOutput:
  given OutputOps[FlagOutput] with
    def valid(pointer: Pointer): FlagOutput                           = FlagOutput(true)
    def invalid(error: ValidationError, pointer: Pointer): FlagOutput = FlagOutput(false)

    def all(os: Seq[FlagOutput], pointer: Pointer): FlagOutput = FlagOutput(os.forall(_.valid))

    extension (o: FlagOutput)
      def not(pointer: Pointer): FlagOutput                 = o.copy(valid = !o.valid)
      def isValid: Boolean                                  = o.valid
      def withAnnotation(annotation: Evaluated): FlagOutput = o.copy(annotation = Some(annotation))
      def annotation: Option[Evaluated]                     = o.annotation
