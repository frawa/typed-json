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

object SuggestionChecker {

  def apply(at: Pointer): Checker[SuggestionResult] = Checker(check(at), nested(at))

  private def check(at: Pointer)(check: SimpleCheck)(value: InnerValue): Checked[SuggestionResult] = {
    val inside = Pointer.dropPrefix(at, value.pointer)
    inside
      .map(inside => {
        val suggestions = suggestFor(check)
        Checked(true, Seq(SuggestionResult(suggestions)))
      })
      .getOrElse {
        val checked = ValidationChecker().check(check)(value)
        Checked(checked.valid, Seq())
      }
  }

  private def nested(
      at: Pointer
  )(check: NestingCheck)(checked: Seq[Checked[SuggestionResult]])(value: InnerValue): Checked[SuggestionResult] = {
    if (at == value.pointer) {
      val suggestions = suggestFor(check)
      Checked(true, Seq(SuggestionResult(suggestions)))
    } else {
      val results = checked.flatMap(_.results)
      Checked(true, results)
    }
  }

  private def suggestFor(check: Check): Seq[Value] = {
    check match {
      case NullTypeCheck    => Seq(NullValue)
      case BooleanTypeCheck => Seq(BoolValue(true))
      case StringTypeCheck  => Seq(StringValue(""))
      case NumberTypeCheck  => Seq(NumberValue(0))
      case ArrayTypeCheck   => Seq(ArrayValue(Seq()))
      case ObjectTypeCheck  => Seq(ObjectValue(Map()))
      // case ArrayItemsCheck(items) =>
      //   items
      //     .map { checks =>
      //       checks.checks
      //         .flatMap(suggestFor(_))
      //         .map(v => ArrayValue(Seq(v)))
      //     }
      //     .getOrElse(Seq(ArrayValue(Seq())))
      case ObjectPropertiesCheck(properties) =>
        properties.flatMap { case (prop, checks) =>
          checks.checks
            .flatMap(suggestFor(_))
            .map(v => ObjectValue(Map(prop -> v)))
        }.toSeq
      case ObjectRequiredCheck(required) => Seq(ObjectValue(Map.from(required.map((_, NullValue)))))
      case TrivialCheck(valid)           => Seq()
      // case AllOfCheck(checks)            => checks.flatMap(_.checks).flatMap(suggestFor(_))
      // case AnyOfCheck(checks)            => checks.flatMap(_.checks).flatMap(suggestFor(_))
      // case OneOfCheck(checks)            => checks.flatMap(_.checks).flatMap(suggestFor(_))
      // case IfThenElseCheck(ifChecks, thenChecks, elseChecks) =>
      //   Seq(ifChecks, thenChecks, elseChecks)
      //     .flatMap(identity)
      //     .flatMap(_.checks)
      //     .flatMap(suggestFor(_))
      case UnionTypeCheck(checks) => checks.flatMap(suggestFor(_))
      case EnumCheck(values)      => values
      case _                      => Seq()
    }
  }
}
