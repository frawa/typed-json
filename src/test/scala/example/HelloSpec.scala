package example

import munit._

class HelloSpec extends FunSuite {
  test("say hello") {
    assert(Hello.greeting == "hello")
  }
}
