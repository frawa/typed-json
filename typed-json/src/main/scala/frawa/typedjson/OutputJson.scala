package frawa.typedjson

import frawa.typedjson.parser.Value
import frawa.typedjson.parser.Value._
import frawa.typedjson.validation.ValidationError

object OutputJson {

  def flag(validation: TypedJson.Validation): Value = {
    ObjectValue(Map("valid" -> BoolValue(validation.valid)))
  }

  def basic(validation: TypedJson.Validation): Value = {
    val errors = if (validation.output.errors.isEmpty) {
      Map()
    } else {
      Map("errors" -> ArrayValue(validation.output.errors.map(toJson)))
    }
    ObjectValue(Map("valid" -> BoolValue(validation.valid)) ++ errors)
  }

  private def toJson(error: TypedJson.Error): Value = {
    ObjectValue(
      Map(
        "error" -> StringValue(toMessage(error.error)),
        // "keywordLocation"         -> StringValue(""),
        // "absoluteKeywordLocation" -> StringValue(""),
        "instanceLocation" -> StringValue(error.pointer.toString)
      )
    )
  }

  private def toMessage(error: ValidationError): String = {
    error.toString
  }
}
