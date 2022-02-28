package frawa.typedjson

import munit.FunSuite

import frawa.typedjson.parser.Value._
import frawa.typedjson.validation.ValidationResult
import frawa.typedjson.keywords.Result
import frawa.typedjson.validation.FalseSchemaReason

class OutputJsonTest extends FunSuite {

  test("flag valid") {
    assertEquals(
      OutputJson.flag(TypedJson.Validation(true, TypedJson.Output(Nil))),
      ObjectValue(Map("valid" -> BoolValue(true)))
    )
  }

  test("flag invalid") {
    assertEquals(
      OutputJson.flag(TypedJson.Validation(false, TypedJson.Output(Nil))),
      ObjectValue(Map("valid" -> BoolValue(false)))
    )
  }

  test("basic valid") {
    assertEquals(
      OutputJson.basic(TypedJson.Validation(true, TypedJson.Output(Nil))),
      ObjectValue(Map("valid" -> BoolValue(true)))
    )
  }

  test("basic errors") {
    assertEquals(
      OutputJson.basic(
        TypedJson.Validation(false, TypedJson.Output(Result.valid(ValidationResult.invalid(FalseSchemaReason()))))
      ),
      ObjectValue(
        Map(
          "valid" -> BoolValue(false),
          "errors" -> ArrayValue(
            Seq(
              ObjectValue(
                properties = Map(
                  "error"            -> StringValue("FalseSchemaReason()"),
                  "instanceLocation" -> StringValue("")
                )
              )
            )
          )
        )
      )
    )
  }

}
