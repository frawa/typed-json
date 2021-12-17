package frawa.typedjson

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("TmpMain")
object TmpMain {
  @JSExport
  def hello(): Unit = {
    println("Hello world!")
  }
}
//}
//
//@JSExportAll
//class TmpMainClass {
//  val someval: String = "some starter class value"
//
//  def greet(name: String): Unit = {
//    js.Dynamic.global.global.console.log(s"Hello, $name!")
//  }
//}
//
//@JSExportAll
//object Starter extends StarterObject {
//  val someval: String = "some starter object value"
//
//  def greet(name: String): Unit = {
//    js.Dynamic.global.global.console.log(s"Hello, $name!")
//  }
//
//  def main(args: Array[String]): Unit = {
//    js.Dynamic.global.global.console.log("Hello from Scala.js starter object!")
//    js.Dynamic.global.global.Starter = this.asInstanceOf[js.Any];
//    js.Dynamic.global.global.StarterClassInstance = new StarterClass().asInstanceOf[js.Any];
//  }
//}
//
//@JSExportAll
//class StarterClass {
//  val someval: String = "some starter class value"
//
//  def greet(name: String): Unit = {
//    js.Dynamic.global.global.console.log(s"Hello, $name!")
//  }
//}
