package frawa.typedjson.schema

import frawa.typedjson.parser.Value
import frawa.typedjson.parser.ZioParser
import frawa.typedjson.parser.Parser
import frawa.typedjson.parser.NumberValue
import frawa.typedjson.parser.BoolValue
import frawa.typedjson.parser.NullValue
import frawa.typedjson.parser.StringValue
import frawa.typedjson.parser.ArrayValue
import frawa.typedjson.parser.ObjectValue
import scala.reflect.internal.Reporter
import java.net.URI
import scala.reflect.ClassTag

case class ValidationChecker() extends Checker[ValidationResult] {

  override def init: ValidationResult = ValidationValid

  override def check(check: Check): Value => ValidationResult = { value: Value =>
    (check, value) match {
      case _ => ValidationInvalid(Seq(WithPointer(UnsupportedCheck(check))))
    }
  }

}
