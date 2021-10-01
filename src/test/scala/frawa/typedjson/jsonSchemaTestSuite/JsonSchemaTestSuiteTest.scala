package frawa.typedjson.schemaSpec

import munit.FunSuite
import frawa.typedjson.schema.SchemaValue
import frawa.typedjson.parser.ZioParser
import frawa.typedjson.parser.Parser
import scala.io.Source
import frawa.typedjson.schema.TestUtil
import frawa.typedjson.schema.Processor
import frawa.typedjson.schema.ValidationChecker
import frawa.typedjson.schema.InnerValue
import frawa.typedjson.schema.Pointer
import frawa.typedjson.schema.LoadedSchemasResolver
import frawa.typedjson.parser.Value
import frawa.typedjson.schema.Checked
import frawa.typedjson.schema.ValidationResult
import java.nio.file.Paths
import java.nio.file.Files
import java.nio.file.Path
import frawa.typedjson.parser.ObjectValue
import frawa.typedjson.parser.ArrayValue
import frawa.typedjson.parser.StringValue
import frawa.typedjson.parser.BoolValue

class JsonSchemaTestSuiteTest extends FunSuite {
  implicit val zioParser = new ZioParser()

  val jsonSchemaTestSuiteRoot = Paths.get("./JSON-Schema-Test-Suite/tests")
  val draft                   = "draft2020-12"

  // TODO unskip
  val skip = Set(
    "anchor.json",
    "content.json",
    "defs.json",
    "dynamicRef.json",
    "format.json",
    "id.json",
    "infinite-loop-detection.json",
    "items.json",
    "ref.json", // stackoverflow
    "refRemote.json",
    "unevaluatedItems.json",
    "unevaluatedProperties.json",
    "unknownKeyword.json"
  )

  val takeOnly: Option[Int] = None
  // val takeOnly: Option[Int] = Some(33)

  override def munitIgnore: Boolean = !Files.exists(jsonSchemaTestSuiteRoot)

  private def listTests(draft: String): Seq[Path] = {
    import scala.jdk.CollectionConverters._

    val draftPath = jsonSchemaTestSuiteRoot.resolve(draft)
    Files
      .list(draftPath)
      .iterator
      .asScala
      .toSeq
      .filter(_.getFileName.toString.endsWith(".json"))
      .sortBy(_.getFileName.toString)
      .toSeq
  }

  private def check(path: Path): Unit = {
    val text = Source.fromFile(path.toFile).getLines.mkString("\n")
    checkSuite(path)(TestUtil.parseJsonValue(text))
  }

  private def checkSuite(path: Path)(testSuiteValue: Value): Unit = {
    testSuiteValue match {
      case ArrayValue(tests) => tests.foreach(checkTest(path))
      case _                 => fail("invalid test json suite")
    }
  }

  private def checkTest(path: Path)(testValue: Value): Unit = {
    testValue match {
      case ObjectValue(properties) =>
        val StringValue(description) = properties("description")
        val testName                 = s"${path.getFileName} - ${description}"

        test(testName) {
          val schema            = properties("schema")
          val ArrayValue(tests) = properties("tests")

          val schemaValue = SchemaValue(schema)
          val processor0  = Processor(schemaValue)(ValidationChecker())
          val processor = processor0.swap
            .map(message => fail("no processor", clues(clue(message))))
            .swap
            .toOption
            .get

          assertEquals(processor.ignoredKeywords, Set.empty[String])

          tests.foreach(assertOne(processor))
        }
      case _ => fail("invalid test json")
    }
  }

  private def assertOne(processor: Processor[ValidationResult]): Value => Unit = { value =>
    val ObjectValue(properties)  = value
    val data                     = properties("data")
    val StringValue(failMessage) = properties("description")
    val BoolValue(expected)      = properties("valid")
    val checked                  = processor.process(InnerValue(data))

    if (checked.valid != expected) {
      implicit val loc = munit.Location.empty
      if (!checked.valid) {
        assertEquals(checked.results, Seq(), failMessage)
      } else {
        fail(failMessage)
      }
    }
  }

  protected def checkDraft(draft: String): Unit = {
    val list = listTests(draft)
      .filterNot(p => skip.contains(p.getFileName.toString))
    takeOnly
      .map(takeOnly => list.take(takeOnly))
      .getOrElse(list)
      .foreach(check)
  }

  checkDraft(draft)

}
