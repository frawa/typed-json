package frawa.typedjson.eval

import frawa.typedjson.validation.ValidationAnnotation
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.validation.ValidationError
import frawa.typedjson.keywords.SchemaProblems

// see https://json-schema.org/draft/2020-12/json-schema-core.html#name-flag

case class FlagOutput(valid: Boolean)

object FlagOutput:
  given OutputOps[FlagOutput] with
    def valid(pointer: Pointer): FlagOutput                                   = FlagOutput(true)
    def valid(annotation: ValidationAnnotation, pointer: Pointer): FlagOutput = FlagOutput(true)
    def invalid(error: ValidationError, pointer: Pointer): FlagOutput         = FlagOutput(false)
    def invalid(problems: SchemaProblems): FlagOutput                         = FlagOutput(false)

    def all(os: Seq[FlagOutput], pointer: Pointer): FlagOutput = FlagOutput(os.forall(_.valid))
    def any(os: Seq[FlagOutput], pointer: Pointer): FlagOutput = ???
    def one(os: Seq[FlagOutput], pointer: Pointer): FlagOutput = ???
    def contains(os: Seq[FlagOutput], min: Option[Int], max: Option[Int], pointer: Pointer): FlagOutput = ???

    extension (o: FlagOutput)
      def not(pointer: Pointer): FlagOutput = o.copy(valid = !o.valid)
      def isValid: Boolean                  = ???
