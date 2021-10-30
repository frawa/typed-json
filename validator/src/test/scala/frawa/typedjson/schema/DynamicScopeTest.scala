package frawa.typedjson.schema

import munit.FunSuite
import frawa.typedjson.parser.ZioParser
import frawa.typedjson.parser.Parser
import java.net.URI

class DynamicScopeTest extends FunSuite {

  test("empty") {
    assertEquals(DynamicScope.empty.candidates, Seq())
  }

  test("push first pointer segment") {
    val scope = DynamicScope.empty.push("toto")
    assertEquals(scope.uris, Seq(URI.create("#/toto")))
  }

  test("push more pointer segments") {
    val scope = DynamicScope.empty
      .push("toto")
      .push("titi")
    assertEquals(
      scope.uris,
      Seq(
        URI.create("#/toto"),
        URI.create("#/toto/titi")
      )
    )
  }

  test("push uri") {
    val scope = DynamicScope.empty
      .push(URI.create("toto"))
      .push(URI.create("titi"))
    assertEquals(
      scope.uris,
      Seq(
        URI.create("toto"),
        URI.create("titi")
      )
    )
  }

  test("push mixed") {
    val scope = DynamicScope.empty
      .push(URI.create("toto"))
      .push("foo")
      .push(URI.create("titi"))
      .push("bar")
    assertEquals(
      scope.uris,
      Seq(
        URI.create("toto"),
        URI.create("toto#/foo"),
        URI.create("titi"),
        URI.create("titi#/bar")
      )
    )
  }

  test("candidates without fragement") {
    val scope = DynamicScope.empty
      .push(URI.create("toto"))
      .push("foo")
      .push(URI.create("titi"))
      .push("bar")
    assertEquals(
      scope.candidates,
      Seq(
        URI.create("toto"),
        URI.create("titi")
      )
    )
  }

}
