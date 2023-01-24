package frawa.typedjson.eval

import frawa.typedjson.keywords.Keyword
import frawa.typedjson.parser.Value
import frawa.typedjson.validation.TypeMismatch
import scala.reflect.TypeTest
import frawa.typedjson.parser.Value._
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.validation.FalseSchemaReason

class Validate[O: OutputOps]:
  import Eval.Fun

  val nullTypeMismatch    = TypeMismatch[NullValue.type]("null")
  val booleanTypeMismatch = TypeMismatch[BoolValue]("boolean")
  val stringTypeMismatch  = TypeMismatch[StringValue]("string")
  val numberTypeMismatch  = TypeMismatch[NumberValue]("number")
  val arrayTypeMismatch   = TypeMismatch[ArrayValue]("array")
  val objectTypeMismatch  = TypeMismatch[ObjectValue]("object")

  private val ops = summon[OutputOps[O]]

  def validateType[T <: Value](error: TypeMismatch[T])(using TypeTest[Value, T]): Fun[O] = value =>
    value match
      case _: T => ops.valid
      case _    => ops.invalid(error, Pointer.empty)

  def validateTrivial(valid: Boolean): Fun[O] = value =>
    if valid then ops.valid
    else ops.invalid(FalseSchemaReason(), Pointer.empty)

  def validateNot(f: Fun[O]): Fun[O]         = ???
  def validateUnion(fs: Seq[Fun[O]]): Fun[O] = ???
  def validateAll(fs: Seq[Fun[O]]): Fun[O]   = value => ops.all(fs.map(_(value)))
