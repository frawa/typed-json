package frawa.typedjson.schema

import frawa.typedjson.parser.Value
import shapeless.the
import frawa.typedjson.parser.ObjectValue
import frawa.typedjson.parser.NullValue
import frawa.typedjson.parser.ArrayValue
import frawa.typedjson.parser.BoolValue
import frawa.typedjson.parser.StringValue
import frawa.typedjson.parser.NumberValue

case class SuggestionResult(suggestions: Seq[Value])

class SuggestionChecker extends Checker[SuggestionResult] {

  override def check(checks: Checks)(value: Value): SuggestionResult = {
    val suggestions = checks.checks.flatMap(suggestFor(_)).distinct
    SuggestionResult(suggestions)
  }

  private def suggestFor(check: Check): Seq[Value] = {
    check match {
      case NullTypeCheck    => Seq(NullValue)
      case BooleanTypeCheck => Seq(BoolValue(true))
      case StringTypeCheck  => Seq(StringValue(""))
      case NumberTypeCheck  => Seq(NumberValue(0))
      case ArrayTypeCheck   => Seq(ArrayValue(Seq()))
      case ObjectTypeCheck  => Seq(ObjectValue(Map()))
      case ArrayItemsCheck(items) =>
        items
          .map { checks =>
            checks.checks
              .flatMap(suggestFor(_))
              .map(v => ArrayValue(Seq(v)))
          }
          .getOrElse(Seq(ArrayValue(Seq())))
      case ObjectPropertiesCheck(properties) =>
        properties.flatMap { case (prop, checks) =>
          checks.checks
            .flatMap(suggestFor(_))
            .map(v => ObjectValue(Map(prop -> v)))
        }.toSeq
      case ObjectRequiredCheck(required) => Seq(ObjectValue(Map.from(required.map((_, NullValue)))))
      case TrivialCheck(valid)           => Seq()
      case NotCheck(checks)              => Seq()
      case AllOfCheck(checks)            => checks.flatMap(_.checks).flatMap(suggestFor(_))
      case AnyOfCheck(checks)            => checks.flatMap(_.checks).flatMap(suggestFor(_))
      case OneOfCheck(checks)            => checks.flatMap(_.checks).flatMap(suggestFor(_))
      case IfThenElseCheck(ifChecks, thenChecks, elseChecks) =>
        Seq(ifChecks, thenChecks, elseChecks)
          .flatMap(identity)
          .flatMap(_.checks)
          .flatMap(suggestFor(_))
      case UnionTypeCheck(checks) => checks.flatMap(suggestFor(_))
      case EnumCheck(values)      => values
      case _                      => Seq()
    }
  }
}
