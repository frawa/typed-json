/*
 * Copyright 2021 Frank Wagner
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package frawa.typedjson.schema

import frawa.typedjson.parser.ArrayValue
import frawa.typedjson.parser.BoolValue
import frawa.typedjson.parser.NumberValue
import frawa.typedjson.parser.ObjectValue
import frawa.typedjson.parser.StringValue
import frawa.typedjson.parser.Value

import java.net.URI
import scala.reflect.ClassTag

case class InnerValue(value: Value, pointer: Pointer = Pointer.empty)

case class SchemaQuality(errors: Seq[SchemaError], ignoredKeywords: Set[String], pointer: Pointer = Pointer.empty) {
  def addIgnoredKeywords(ignoredKeywords: Set[String]): SchemaQuality =
    copy(ignoredKeywords = this.ignoredKeywords ++ ignoredKeywords)

  def addErrors(errors: Seq[SchemaError]): SchemaQuality =
    copy(errors = this.errors ++ errors)

  def combine(other: SchemaQuality): SchemaQuality =
    copy(errors = this.errors ++ other.errors, ignoredKeywords = this.ignoredKeywords ++ other.ignoredKeywords)
}

object SchemaQuality {
  val empty: SchemaQuality = SchemaQuality(Seq.empty, Set.empty)
}

case class SchemaError(message: String, pointer: Pointer = Pointer.empty) {
  def prefix(prefix: Pointer): SchemaError = SchemaError(message, prefix / pointer)
}

sealed trait Check
sealed trait SimpleCheck  extends Check
sealed trait TypeCheck    extends SimpleCheck
sealed trait NestingCheck extends Check

case class TrivialCheck(v: Boolean)                              extends SimpleCheck
case object NullTypeCheck                                        extends TypeCheck
case object BooleanTypeCheck                                     extends TypeCheck
case object StringTypeCheck                                      extends TypeCheck
case object NumberTypeCheck                                      extends TypeCheck
case object IntegerTypeCheck                                     extends TypeCheck
case object ArrayTypeCheck                                       extends TypeCheck
case object ObjectTypeCheck                                      extends TypeCheck
case class ObjectRequiredCheck(names: Seq[String])               extends SimpleCheck
case class NotCheck(checks: Checks)                              extends NestingCheck
case class AllOfCheck(checks: Seq[Checks])                       extends NestingCheck
case class AnyOfCheck(checks: Seq[Checks])                       extends NestingCheck
case class OneOfCheck(checks: Seq[Checks])                       extends NestingCheck
case class UnionTypeCheck(checks: Seq[Checks.CheckWithLocation]) extends NestingCheck
case class EnumCheck(values: Seq[Value])                         extends SimpleCheck
case class ArrayItemsCheck(
    items: Option[Checks] = None,
    prefixItems: Seq[Checks] = Seq()
    // unevaluated: Option[Checks] = None
) extends NestingCheck
case class ObjectPropertiesCheck(
    properties: Map[String, Checks] = Map(),
    patternProperties: Map[String, Checks] = Map(),
    additionalProperties: Option[Checks] = None
) extends NestingCheck
case class IfThenElseCheck(
    ifChecks: Option[Checks] = None,
    thenChecks: Option[Checks] = None,
    elseChecks: Option[Checks] = None
) extends NestingCheck
case class PatternCheck(pattern: String)                                                            extends SimpleCheck
case class FormatCheck(format: String)                                                              extends SimpleCheck
case class MinimumCheck(min: BigDecimal, exclude: Boolean = false)                                  extends SimpleCheck
case class UniqueItemsCheck(unique: Boolean)                                                        extends SimpleCheck
case class PropertyNamesCheck(checks: Checks)                                                       extends NestingCheck
case class LazyResolveCheck(val resolved: URI, val resolve: () => Either[Seq[SchemaError], Checks]) extends NestingCheck
case class MultipleOfCheck(n: BigDecimal)                                                           extends SimpleCheck
case class MaximumCheck(max: BigDecimal, exclude: Boolean = false)                                  extends SimpleCheck
case class MaxLengthCheck(max: BigDecimal)                                                          extends SimpleCheck
case class MinLengthCheck(min: BigDecimal)                                                          extends SimpleCheck
case class MaxItemsCheck(max: BigDecimal)                                                           extends SimpleCheck
case class MinItemsCheck(min: BigDecimal)                                                           extends SimpleCheck
case class MaxPropertiesCheck(max: BigDecimal)                                                      extends SimpleCheck
case class MinPropertiesCheck(min: BigDecimal)                                                      extends SimpleCheck
case class DependentRequiredCheck(required: Map[String, Seq[String]])                               extends SimpleCheck
case class DependentSchemasCheck(checks: Map[String, Checks])                                       extends NestingCheck
case class ContainsCheck(schema: Option[Checks] = None, min: Option[Int] = None, max: Option[Int] = None)
    extends NestingCheck
case class UnevaluatedItemsCheck(pushedChecks: Checks, unevaluated: Checks)      extends NestingCheck
case class UnevaluatedPropertiesCheck(pushedChecks: Checks, unevaluated: Checks) extends NestingCheck

case class Checked[R](
    valid: Boolean,
    results: Seq[R],
    annotations: Seq[Checked.Annotation] = Seq.empty,
    validation: SchemaQuality = SchemaQuality.empty,
    count: Int = 1
) {
  def add(others: Seq[Checked[R]]): Checked[R] =
    this
      .copy(count = this.count + Checked.count(others))
      .addAnnotations(others.flatMap(_.annotations))
      .addValidations(others.map(_.validation))
  def add(validation: SchemaQuality): Checked[R] = this.copy(validation = this.validation.combine(validation))
  private def addValidations(validations: Seq[SchemaQuality]): Checked[R] =
    this.copy(validation = validations.foldLeft(this.validation)(_.combine(_)))

  def add(annotation: Checked.Annotation): Checked[R] = this.copy(annotations = this.annotations :+ annotation)
  private def addAnnotations(annotations: Seq[Checked.Annotation]) =
    this.copy(annotations = this.annotations ++ annotations)
}

// TODO rename to something like "Annotation"
sealed trait Observation2
case class EvaluatedIndices(indices: Seq[Int])          extends Observation2
case class EvaluatedProperties(properties: Set[String]) extends Observation2

object Checked {
  type Annotation = WithPointer[Observation2]

  def apply[R](valid: Boolean, result: R): Checked[R] = Checked[R](valid, Seq(result))
  def valid[R]: Checked[R]                            = Checked[R](true, Seq())
  def valid[R](result: R): Checked[R]                 = Checked[R](true, Seq(result))
  def invalid[R]: Checked[R]                          = Checked[R](false, Seq())
  def invalid[R](result: R): Checked[R]               = Checked[R](false, Seq(result))

  def merge[R](checked: Seq[Checked[R]]): Checked[R] = {
    val valid           = checked.forall(_.valid)
    val results: Seq[R] = checked.flatMap(_.results)
    val validation      = checked.map(_.validation).reduceOption(_.combine(_)).getOrElse(SchemaQuality.empty)
    val annotations     = checked.flatMap(_.annotations)
    Checked(valid, results, annotations, validation, 1 + count(checked))
  }
  def count[R](checked: Seq[Checked[R]]): Int = checked.map(_.count).sum
}

case class Checks(
    schema: SchemaValue,
    checks: Seq[Checks.CheckWithLocation] = Seq.empty[Checks.CheckWithLocation],
    ignoredKeywords: Set[String] = Set.empty
) {
  import SeqUtil._
  import Checks._

  private def add(check: Check)(implicit scope: DynamicScope): Checks =
    this.copy(checks = checks :+ localized(check, scope))

  private def addAll(
      values: Seq[SchemaValue]
  )(f: Seq[Checks] => Check)(implicit resolver: SchemaResolver, scope: DynamicScope): Either[SchemaErrors, Checks] = {
    val checks0 = values.zipWithIndex.map { case (v, i) => Checks.parseKeywords(v, scope.push(i)) }.toSeq
    for {
      checks <- sequenceAllLefts(checks0)
    } yield {
      add(f(checks)).withIgnored(checks.flatMap(_.ignoredKeywords).toSet)
    }
  }

  type SchemaErrors = Checks.SchemaErrors

  private def withKeyword(keyword: String, value: Value, scope: DynamicScope)(implicit
      resolver: SchemaResolver
  ): Either[SchemaErrors, Checks] = {
    implicit val scope1 = scope.push(keyword)
    (keyword, value) match {
      // TODO validation vocabulary
      case ("type", StringValue(typeName)) =>
        Right(
          getTypeCheck(typeName)
            .map(add(_))
            .getOrElse(withIgnored(s"${keyword}-${typeName}"))
        )

      // TODO validation vocabulary
      case ("type", ArrayValue(values)) => {
        def typeNames = Value.asStrings(values)
        def checks = typeNames
          .flatMap(getTypeCheck(_))
          .map(localized(_, scope1))
        Right(add(UnionTypeCheck(checks)))
      }

      case ("not", value) =>
        for {
          checks <- Checks.parseKeywords(SchemaValue(value), scope1)
        } yield {
          add(NotCheck(checks))
        }

      case ("items", value) =>
        for {
          checks <- Checks.parseKeywords(SchemaValue(value), scope1)
        } yield {
          updateCheck(ArrayItemsCheck())(check => check.copy(items = Some(checks)))
        }

      case ("prefixItems", ArrayValue(vs)) =>
        val checks0 = vs.map(v => Checks.parseKeywords(SchemaValue(v), scope1))
        for {
          checks <- sequenceAllLefts(checks0)
        } yield {
          updateCheck(ArrayItemsCheck())(check => check.copy(prefixItems = checks))
        }

      case ("unevaluatedItems", v) => {
        for {
          checks <- Checks.parseKeywords(SchemaValue(v), scope1)
        } yield {
          Checks(schema).add(UnevaluatedItemsCheck(this, checks))(scope)
        }
      }

      case ("properties", ObjectValue(properties)) =>
        mapChecksFor(properties, scope1) { checks =>
          updateCheck(ObjectPropertiesCheck())(check => check.copy(properties = check.properties ++ checks))
        }

      case ("patternProperties", ObjectValue(properties)) =>
        mapChecksFor(properties, scope1) { checks =>
          updateCheck(ObjectPropertiesCheck())(check => check.copy(patternProperties = checks))
        }

      case ("additionalProperties", value) =>
        for {
          checks <- Checks.parseKeywords(SchemaValue(value), scope1)
        } yield {
          updateCheck(ObjectPropertiesCheck())(check => check.copy(additionalProperties = Some(checks)))
        }

      case ("unevaluatedProperties", v) => {
        for {
          checks <- Checks.parseKeywords(SchemaValue(v), scope1)
        } yield {
          Checks(schema).add(UnevaluatedPropertiesCheck(this, checks))(scope)
        }
      }

      case ("required", ArrayValue(values)) => {
        def names = Value.asStrings(values)
        Right(add(ObjectRequiredCheck(names)))
      }

      case ("allOf", ArrayValue(values)) => {
        addAll(values.map(SchemaValue(_)))(AllOfCheck)
      }

      case ("anyOf", ArrayValue(values)) => {
        addAll(values.map(SchemaValue(_)))(AnyOfCheck)
      }

      case ("oneOf", ArrayValue(values)) => {
        addAll(values.map(SchemaValue(_)))(OneOfCheck)
      }

      case ("if", value) =>
        updateChecks(SchemaValue(value))(IfThenElseCheck()) { (checks, check) =>
          check.copy(ifChecks = Some(checks))
        }

      case ("then", value) =>
        updateChecks(SchemaValue(value))(IfThenElseCheck()) { (checks, check) =>
          check.copy(thenChecks = Some(checks))
        }

      case ("else", value) =>
        updateChecks(SchemaValue(value))(IfThenElseCheck()) { (checks, check) =>
          check.copy(elseChecks = Some(checks))
        }

      // TODO validation vocabulary
      case ("enum", ArrayValue(values)) => {
        Right(add(EnumCheck(values)))
      }

      // TODO validation vocabulary
      case ("const", value) => {
        Right(add(EnumCheck(Seq(value))))
      }

      case ("$id", StringValue(_)) => {
        // handled during load
        Right(this)
      }

      case ("$anchor", StringValue(_)) => {
        // handled during load
        Right(this)
      }

      case ("$dynamicAnchor", StringValue(_)) => {
        // handled during load
        Right(this)
      }

      case ("$defs", ObjectValue(_)) => {
        // handled during load
        Right(this)
      }

      case ("$ref", StringValue(ref)) => {
        for {
          resolution <- resolver
            .resolveRef(ref)
            .map(Right(_))
            // .getOrElse(throw new IllegalStateException(s"$ref not found"))
            .getOrElse(Left(Seq(SchemaError(s"""missing reference "${ref}""""))))
          check = lazyResolveCheck(resolution, scope1)
        } yield {
          add(check)
        }
      }

      case ("$dynamicRef", StringValue(ref)) => {
        for {
          resolution <- resolver
            .resolveDynamicRef(ref, scope)
            .map(Right(_))
            .getOrElse(Left(Seq(SchemaError(s"""missing dynamic reference "${ref}""""))))
          check = lazyResolveCheck(resolution, scope1)
        } yield {
          add(check)
        }
      }

      case ("$comment", StringValue(_)) => {
        // only for schema authors and readers
        Right(this)
      }

      case ("title", StringValue(_)) => {
        // ignore annotations
        Right(this)
      }

      case ("default", _) => {
        // ignore annotations
        Right(this)
      }

      case ("description", StringValue(_)) => {
        // ignore annotations
        Right(this)
      }

      // TODO validation vocabulary
      case ("pattern", StringValue(pattern)) => {
        Right(add(PatternCheck(pattern)))
      }

      // TODO validation vocabulary
      case ("format", StringValue(format)) => {
        Right(add(FormatCheck(format)))
      }

      // TODO validation vocabulary
      case ("minimum", NumberValue(v)) => {
        Right(add(MinimumCheck(v)))
      }

      // TODO validation vocabulary
      case ("exclusiveMinimum", NumberValue(v)) => {
        Right(add(MinimumCheck(v, true)))
      }

      // TODO validation vocabulary
      case ("minItems", NumberValue(v)) if v >= 0 => {
        Right(add(MinItemsCheck(v)))
      }

      // TODO validation vocabulary
      case ("uniqueItems", BoolValue(v)) => {
        Right(add(UniqueItemsCheck(v)))
      }

      case ("propertyNames", value) =>
        for {
          checks <- Checks.parseKeywords(SchemaValue(value), scope1)
        } yield {
          add(PropertyNamesCheck(checks))
        }

      // TODO validation vocabulary
      case ("multipleOf", NumberValue(v)) if v > 0 => {
        Right(add(MultipleOfCheck(v)))
      }

      // TODO validation vocabulary
      case ("maximum", NumberValue(v)) => {
        Right(add(MaximumCheck(v)))
      }

      // TODO validation vocabulary
      case ("exclusiveMaximum", NumberValue(v)) => {
        Right(add(MaximumCheck(v, true)))
      }

      // TODO validation vocabulary
      case ("maxLength", NumberValue(v)) if v >= 0 => {
        Right(add(MaxLengthCheck(v)))
      }

      // TODO validation vocabulary
      case ("minLength", NumberValue(v)) if v >= 0 => {
        Right(add(MinLengthCheck(v)))
      }

      // TODO validation vocabulary
      case ("maxItems", NumberValue(v)) if v >= 0 => {
        Right(add(MaxItemsCheck(v)))
      }

      // TODO validation vocabulary
      case ("maxProperties", NumberValue(v)) if v >= 0 => {
        Right(add(MaxPropertiesCheck(v)))
      }

      // TODO validation vocabulary
      case ("minProperties", NumberValue(v)) if v >= 0 => {
        Right(add(MinPropertiesCheck(v)))
      }

      // TODO validation vocabulary
      case ("dependentRequired", ObjectValue(v)) => {
        val vv = v.view
          .map {
            case (p, ArrayValue(vs)) =>
              Some(
                (
                  p,
                  vs.flatMap {
                    case StringValue(value) => Some(value)
                    case _                  => None
                  }
                )
              )
            case _ => None
          }
          .flatten
          .toMap
        Right(add(DependentRequiredCheck(vv)))
      }

      case ("dependentSchemas", ObjectValue(ps)) => {
        val checks0 = ps.view.map { case (p, v) =>
          Checks
            .parseKeywords(SchemaValue(v), scope1)
            .map(p -> _)
        }.toSeq
        for {
          checks1 <- sequenceAllLefts(checks0).map(_.toMap)
        } yield {
          add(DependentSchemasCheck(checks1))
        }
      }

      case ("contains", v) =>
        for {
          checks <- Checks.parseKeywords(SchemaValue(v), scope1)
        } yield {
          updateCheck(ContainsCheck())(check => check.copy(schema = Some(checks)))
        }

      // TODO validation vocabulary
      case ("minContains", NumberValue(v)) if v >= 0 =>
        Right(updateCheck(ContainsCheck())(check => check.copy(min = Some(v.toInt))))

      // TODO validation vocabulary
      case ("maxContains", NumberValue(v)) if v >= 0 =>
        Right(updateCheck(ContainsCheck())(check => check.copy(max = Some(v.toInt))))

      // // TODO
      // case ("$vocabulary", v) => {
      //   Right(this)
      // }
      // // TODO
      // case ("$schema", v) => {
      //   Right(this)
      // }
      // // TODO
      // case ("deprecated", v) => {
      //   Right(this)
      // }

      case _ => Right(withIgnored(keyword))
    }
  }

  private def lazyResolveCheck(resolution: SchemaResolver.Resolution, scope: DynamicScope): LazyResolveCheck = {
    val resolveLater = { () =>
      val schema    = resolution._1
      val resolver1 = resolution._2
      Checks.parseKeywords(schema, scope)(resolver1)
    }
    val resolved = resolution._2.base
    LazyResolveCheck(resolved, resolveLater)
  }

  private def mapChecksFor(
      props: Map[String, Value],
      scope: DynamicScope
  )(f: Map[String, Checks] => Checks)(implicit resolver: SchemaResolver): Either[Seq[SchemaError], Checks] = {
    val propChecks = props.view
      .map { case (prop, v) =>
        (prop, Checks.parseKeywords(SchemaValue(v), scope.push(prop)))
      }
      .map {
        case (prop, Right(checks)) => Right((prop, checks))
        case (prop, Left(errors))  => Left(errors.map(_.prefix(Pointer.empty / prop)))
      }
      .toSeq
    for {
      propChecks1 <- sequenceAllLefts(propChecks)
      checks1 = Map.from(propChecks1)
      ignored = checks1.values.flatMap(_.ignoredKeywords).toSet
    } yield {
      f(checks1).withIgnored(ignored)
    }
  }

  private def updateCheck[C <: Check: ClassTag](newCheck: => C)(f: C => C)(implicit scope: DynamicScope): Checks = {
    val checks1: Seq[CheckWithLocation] =
      if (
        checks.exists {
          case UriUtil.WithLocation(uri, check: C) => true
          case _                                   => false
        }
      ) {
        checks
      } else {
        checks :+ localized(newCheck, scope)
      }
    this.copy(checks = checks1.map {
      case UriUtil.WithLocation(uri, check: C) => UriUtil.WithLocation(uri, f(check))
      case c @ _                               => c
    })
  }

  private def updateChecks[C <: Check: ClassTag](
      schema: SchemaValue
  )(
      newCheck: => C
  )(f: (Checks, C) => C)(implicit resolver: SchemaResolver, scope: DynamicScope): Either[SchemaErrors, Checks] = {
    for {
      checks <- Checks.parseKeywords(schema, scope)
    } yield {
      updateCheck(newCheck)(f(checks, _))
    }
  }

  private def withIgnored(keyword: String): Checks =
    this.copy(ignoredKeywords = ignoredKeywords + keyword)

  private def withIgnored(ignored: Set[String]): Checks =
    this.copy(ignoredKeywords = ignoredKeywords.concat(ignored))

  private def getTypeCheck(typeName: String): Option[TypeCheck] =
    typeName match {
      case "null"    => Some(NullTypeCheck)
      case "boolean" => Some(BooleanTypeCheck)
      case "string"  => Some(StringTypeCheck)
      case "number"  => Some(NumberTypeCheck)
      case "integer" => Some(IntegerTypeCheck)
      case "array"   => Some(ArrayTypeCheck)
      case "object"  => Some(ObjectTypeCheck)
      case _         => None
    }
}

case class Checker[R](
    check: SimpleCheck => Processor.ProcessFun[R],
    nested: NestingCheck => Processor.MergeFun[R]
)

object Checks {
  type SchemaErrors      = Seq[SchemaError]
  type CheckWithLocation = UriUtil.WithLocation[Check]

  def parseKeywords(schema: SchemaValue, scope: DynamicScope)(implicit
      resolver: SchemaResolver
  ): Either[SchemaErrors, Checks] = {
    implicit val scope1 = SchemaValue
      .id(schema)
      .map(id => scope.push(resolver.absolute(id)))
      .getOrElse(scope)
    val resolver1 = SchemaValue
      .id(schema)
      .map(id => resolver.withBase(resolver.absolute(id)))
      .getOrElse(resolver)
    schema.value match {
      case BoolValue(v) => Right(Checks(schema).add(TrivialCheck(v)))
      case ObjectValue(keywords) =>
        if (keywords.isEmpty) {
          Right(Checks(schema).add(TrivialCheck(true)))
        } else {
          keywords
            .foldLeft[Either[SchemaErrors, Checks]](Right(Checks(schema))) { case (checks, (keyword, value)) =>
              val prefix = Pointer.empty / keyword
              checks
                .flatMap(
                  _.withKeyword(keyword, value, scope1)(resolver1).swap
                    .map(_.map(_.prefix(prefix)))
                    .swap
                )
            }
        }
      case _ => Left(Seq(SchemaError(s"invalid schema ${schema}")))
    }
  }

  def localized(check: Check, scope: DynamicScope): CheckWithLocation = {
    import UriUtil._
    scope.uris.lastOption.map(WithLocation(_, check)).getOrElse(WithLocation(uri("#"), check))
  }
}
