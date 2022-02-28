package frawa.typedjson

import frawa.typedjson.parser.Value
import frawa.typedjson.parser.Value._

object OutputJson {

  def flag(validation: TypedJson.Validation): Value = {
    ObjectValue(Map("valid" -> BoolValue(validation.valid)))
  }
}
