package frawa.typedjson.testutil

import frawa.typedjson.processor._
import frawa.typedjson.testutil

case class ProcessorConversion[V, R](convert: ProcessorConversion.ConvertFun[V, R]) {

  def apply(v: V): Either[SchemaProblems, Processor[R]] = convert(v)

  def mapResult(f: Result[R] => Result[R]): ProcessorConversion[V, R] = {
    ProcessorConversion(convert.andThen(_.map { processor =>
      processor.andThen(f)
    }))
  }
}

object ProcessorConversion {
  type ConvertFun[V, R] = V => Either[SchemaProblems, Processor[R]]

  def toProcessor[R](
      eval: Eval[R],
      vocabulary: Option[Vocabulary] = None,
      lazyResolver: Option[LoadedSchemasResolver.LazyResolver] = None
  ): ProcessorConversion[SchemaValue, R] = testutil.ProcessorConversion({ schema =>
    Keywords(schema, vocabulary, lazyResolver).map(Processor(_, eval))
  })

}
