package frawa.typedjson.schema

import munit.FunSuite
import frawa.typedjson.parser.ZioParser
import frawa.typedjson.parser.Parser
import java.net.URI
import UriUtil._

class DynamicScopeTest extends FunSuite {
  import DynamicScope._

  test("empty") {
    assertEquals(empty.candidates, Seq())
  }

  test("push first pointer segment") {
    val scope = empty.push("toto")
    assertEquals(scope.uris, Seq(uri("#/toto")))
  }

  test("push more pointer segments") {
    val scope = empty
      .push("toto")
      .push("titi")
    assertEquals(
      scope.uris,
      Seq(
        uri("#/toto"),
        uri("#/toto/titi")
      )
    )
  }

  test("push uri") {
    val scope = empty
      .push(uri("toto"))
      .push(uri("titi"))
    assertEquals(
      scope.uris,
      Seq(
        uri("toto"),
        uri("titi")
      )
    )
  }

  test("push mixed") {
    val scope = empty
      .push(uri("toto"))
      .push("foo")
      .push(uri("titi"))
      .push("bar")
    assertEquals(
      scope.uris,
      Seq(
        uri("toto"),
        uri("toto#/foo"),
        uri("titi"),
        uri("titi#/bar")
      )
    )
  }

  test("candidates without fragement") {
    val scope = empty
      .push(uri("toto"))
      .push("foo")
      .push(uri("titi"))
      .push("bar")
    assertEquals(
      scope.candidates,
      Seq(
        uri("toto"),
        uri("titi")
      )
    )
  }

}
