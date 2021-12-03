package frawa.typedjson.testutil

import frawa.typedjson.processor._
import frawa.typedjson.testutil

case class ProcessorFactory[V, R](create: ProcessorFactory.CreateFun[V, R]) {

  def apply(v: V): Either[SchemaProblems, Processor[R]] = create(v)

  def mapResult(f: Result[R] => Result[R]): ProcessorFactory[V, R] = {
    ProcessorFactory(create.andThen(_.map { processor =>
      processor.andThen(f)
    }))
  }
}

object ProcessorFactory {
  type CreateFun[V, R] = V => Either[SchemaProblems, Processor[R]]

  def make[R](
      eval: Eval[R],
      vocabulary: Option[Vocabulary] = None,
      lazyResolver: Option[LoadedSchemasResolver.LazyResolver] = None
  ): ProcessorFactory[SchemaValue, R] = testutil.ProcessorFactory({ schema =>
    Keywords(schema, vocabulary, lazyResolver).map(Processor(_, eval))
  })

}
