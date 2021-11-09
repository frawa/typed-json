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
import frawa.typedjson.schema.SpecMetaSchemas
import TestUtil._
import munit.TestOptions

class JsonSchemaTestSuiteTest extends FunSuite {
  implicit val zioParser = new ZioParser()

  val jsonSchemaTestSuiteRoot = Paths.get("./JSON-Schema-Test-Suite/tests")
  val version                 = "draft2020-12"

  // TODO unskip 'em
  val skip = Set(
    "anchor.json",    // TODO anchor inside enum is not an id
    "content.json",   // TODO keywords contentMediaType, contentEncoding, contentSchema
    "defs.json",      // TODO meta schema with ignored keywords: deprecated, $vocabulary, $schema
    "id.json",        // TODO $id inside enum is not an id
    "refRemote.json", // TODO resolve URI as remote URL
    "unevaluatedProperties.json",
    "unknownKeyword.json" // TODO $id inside an unknown keyword
  )

  val takeOnly: Option[Int] = None
  val only: Option[String]  = None
  // val only: Option[String]            = Some("unevaluatedItems.json")
  val onlyId: Option[String]          = None
  val onlyDescription: Option[String] = None

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

        val onlyTestName = onlyDescription
          .filter(description.startsWith(_))
          .map(_ => testName.only)
          .getOrElse(new TestOptions(testName))

        test(onlyTestName) {
          val schema            = properties("schema")
          val ArrayValue(tests) = properties("tests")

          val id = SchemaValue.id(SchemaValue(schema))
          val includedOnlyId = onlyId
            .flatMap { onlyId =>
              id.map(_ == onlyId)
            }
          assume(includedOnlyId.getOrElse(true), s"excluded by onlyId=${onlyId}")

          val lazyResolver = Some(SpecMetaSchemas.lazyResolver)
          val schemaValue  = SchemaValue(schema)
          withStrictProcessor(ValidationChecker())(schemaValue, lazyResolver) { processor =>
            tests.foreach(assertOne(processor))
          }
        }
      case _ => fail("invalid test json")
    }
  }

  private def assertOne(processor: Processor[ValidationResult]): Value => Unit = { value =>
    val ObjectValue(properties)  = value
    val data                     = properties("data")
    val StringValue(failMessage) = properties("description")
    val BoolValue(expected)      = properties("valid")
    val checked                  = processor(InnerValue(data))

    if (checked.valid != expected) {
      implicit val loc = munit.Location.empty
      if (!checked.valid) {
        assertEquals(checked.results, Seq(), failMessage)
      } else {
        fail("failed", clues(clue(failMessage), clue(expected), clue(checked)))
      }
    }
  }

  protected def checkVersion(version: String): Unit = {
    val list = listTests(version)
      .filterNot(p =>
        skip
          .contains(p.getFileName.toString)
      )
      .filter(p =>
        only
          .map(_ == p.getFileName.toString)
          .getOrElse(true)
      )
    takeOnly
      .map(takeOnly => list.take(takeOnly))
      .getOrElse(list)
      .foreach(check)
  }

  checkVersion(version)

}
