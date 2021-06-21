package frawa.typedjson.schema

import munit.FunSuite

class ValidationResultTest extends FunSuite {
  test("valid") {
    val result = ValidationResult.valid()
    assertEquals(result.valid, true)
    assertEquals(result.errors, Seq())
  }

  test("not valid") {
    val result = ValidationResult.invalid(ValidationError(TypeMismatch("bla")))
    assertEquals(result.valid, false)
    assertEquals(result.errors, Seq(ValidationError(TypeMismatch("bla"))))
  }

  test("valid and valid") {
    val result = ValidationResult.valid().and(ValidationResult.valid())
    assertEquals(result.valid, true)
    assertEquals(result.errors, Seq())
  }

  test("valid and invalid") {
    val result = ValidationResult.valid().and(ValidationResult.invalid(ValidationError(TypeMismatch("bla"))))
    assertEquals(result.valid, false)
    assertEquals(result.errors, Seq(ValidationError(TypeMismatch("bla"))))
  }

  test("invalid and valid") {
    val result = ValidationResult.invalid(ValidationError(TypeMismatch("bla"))).and(ValidationResult.valid())
    assertEquals(result.valid, false)
    assertEquals(result.errors, Seq(ValidationError(TypeMismatch("bla"))))
  }

  test("invalid and invalid") {
    val result = ValidationResult
      .invalid(ValidationError(TypeMismatch("bla")))
      .and(ValidationResult.invalid(ValidationError(TypeMismatch("foo"))))
    assertEquals(result.valid, false)
    assertEquals(result.errors, Seq(ValidationError(TypeMismatch("bla")), ValidationError(TypeMismatch("foo"))))
  }
}
