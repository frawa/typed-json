package frawa.typedjson.suggest

import frawa.typedjson.util.WithPointer
import frawa.typedjson.validation.ValidationError
import frawa.typedjson.validation.ValidationAnnotation
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.keywords.SchemaProblems
import frawa.typedjson.validation.NotOneOf
import frawa.typedjson.validation.NotInvalid
import frawa.typedjson.keywords.Evaluated
import frawa.typedjson.keywords.Keyword
import frawa.typedjson.output.BasicOutput
import frawa.typedjson.output.OutputOps
import frawa.typedjson.suggest.Suggest

case class SuggestOutput(
    basic: BasicOutput,
    keywords: Seq[Keyword] = Seq.empty
)

object SuggestOutput:
  import BasicOutput.given

  def outputOps(at: Pointer): OutputOps[SuggestOutput] =
    SuggestOutputOps(at)

  private class SuggestOutputOps(private val at: Pointer) extends OutputOps[SuggestOutput]:
    private val bops = summon[OutputOps[BasicOutput]]
    private val isAt = Suggest.isAt(at)

    def valid(pointer: Pointer): SuggestOutput =
      SuggestOutput(bops.valid(pointer))

    def invalid(error: ValidationError, pointer: Pointer): SuggestOutput =
      SuggestOutput(bops.invalid(error, pointer))

    def all(os: Seq[SuggestOutput], pointer: Pointer): SuggestOutput =
      SuggestOutput(
        bops.all(os.map(_.basic), pointer),
        os.flatMap(_.keywords)
      )

    extension (o: SuggestOutput)
      def not(pointer: Pointer): SuggestOutput                 = o.copy(basic = o.basic.not(pointer))
      def isValid: Boolean                                     = o.basic.valid
      def withAnnotation(annotation: Evaluated): SuggestOutput = o.copy(basic = o.basic.withAnnotation(annotation))
      def getAnnotations(): Seq[Evaluated]                     = o.basic.annotations
      def forKeyword(k: Keyword): SuggestOutput =
        if isAt(o.basic.pointer) then o.copy(keywords = o.keywords :+ k)
        else o
