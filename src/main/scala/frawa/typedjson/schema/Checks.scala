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
import frawa.typedjson.parser.NullValue
import frawa.typedjson.parser.NumberValue
import frawa.typedjson.parser.ObjectValue
import frawa.typedjson.parser.Parser
import frawa.typedjson.parser.StringValue
import frawa.typedjson.parser.Value
import frawa.typedjson.parser.ZioParser

import java.net.URI
import scala.reflect.ClassTag
import scala.reflect.internal.Reporter

case class InnerValue(value: Value, pointer: Pointer = Pointer.empty)

case class SchemaError(message: String, pointer: Pointer = Pointer.empty) {
  def prefix(prefix: Pointer): SchemaError = SchemaError(message, prefix / pointer)
}

sealed trait Check
sealed trait SimpleCheck  extends Check
sealed trait TypeCheck    extends SimpleCheck
sealed trait NestingCheck extends Check

case class TrivialCheck(v: Boolean)                                                        extends SimpleCheck
case object NullTypeCheck                                                                  extends TypeCheck
case object BooleanTypeCheck                                                               extends TypeCheck
case object StringTypeCheck                                                                extends TypeCheck
case object NumberTypeCheck                                                                extends TypeCheck
case object IntegerTypeCheck                                                               extends TypeCheck
case object ArrayTypeCheck                                                                 extends TypeCheck
case object ObjectTypeCheck                                                                extends TypeCheck
case class ObjectRequiredCheck(names: Seq[String])                                         extends SimpleCheck
case class NotCheck(checks: Checks)                                                        extends NestingCheck
case class AllOfCheck(checks: Seq[Checks])                                                 extends NestingCheck
case class AnyOfCheck(checks: Seq[Checks])                                                 extends NestingCheck
case class OneOfCheck(checks: Seq[Checks])                                                 extends NestingCheck
case class UnionTypeCheck(typeChecks: Seq[TypeCheck])                                      extends SimpleCheck
case class EnumCheck(values: Seq[Value])                                                   extends SimpleCheck
case class ArrayItemsCheck(items: Option[Checks] = None, prefixItems: Seq[Checks] = Seq()) extends NestingCheck
case class ObjectPropertiesCheck(
    properties: Map[String, Checks] = Map(),
    patternProperties: Map[String, Checks] = Map(),
    additionalProperties: Option[Checks] = None
) extends NestingCheck
case class IfThenElseCheck(
    ifChecks: Option[Checks] = None,
    thenChecks: Option[Checks] = None,
    elseChecks: Option[Checks] = None
)                                                                            extends NestingCheck
case class PatternCheck(pattern: String)                                     extends SimpleCheck
case class FormatCheck(format: String)                                       extends SimpleCheck
case class MinimumCheck(min: BigDecimal, exclude: Boolean = false)           extends SimpleCheck
case class UniqueItemsCheck(unique: Boolean)                                 extends SimpleCheck
case class PropertyNamesCheck(checks: Checks)                                extends NestingCheck
case class LazyResolveCheck(resolve: () => Either[Seq[SchemaError], Checks]) extends NestingCheck
case class MultipleOfCheck(n: BigDecimal)                                    extends SimpleCheck
case class MaximumCheck(max: BigDecimal, exclude: Boolean = false)           extends SimpleCheck
case class MaxLengthCheck(max: BigDecimal)                                   extends SimpleCheck
case class MinLengthCheck(min: BigDecimal)                                   extends SimpleCheck
case class MaxItemsCheck(max: BigDecimal)                                    extends SimpleCheck
case class MinItemsCheck(min: BigDecimal)                                    extends SimpleCheck
case class MaxPropertiesCheck(max: BigDecimal)                               extends SimpleCheck
case class MinPropertiesCheck(min: BigDecimal)                               extends SimpleCheck
case class DependentRequiredCheck(required: Map[String, Seq[String]])        extends SimpleCheck
case class DependentSchemasCheck(checks: Map[String, Checks])                extends NestingCheck
case class ContainsCheck(schema: Option[Checks] = None, min: Option[Int] = None, max: Option[Int] = None)
    extends NestingCheck

case class Checked[R](
    valid: Boolean,
    results: Seq[R],
    count: Int,
    ignoredKeywords: Seq[String] = Seq.empty,
    errors: Seq[SchemaError] = Seq.empty
) {
  def add(others: Seq[Checked[R]]): Checked[R]  = Checked(valid, results, count + Checked.count(others))
  def withIgnored(ignored: Set[String])         = this.copy(ignoredKeywords = this.ignoredKeywords ++ ignored)
  def withSchemaErrors(erros: Seq[SchemaError]) = this.copy(errors = this.errors ++ errors)
}

object Checked {
  def apply[R](valid: Boolean, result: R): Checked[R] = Checked[R](valid, Seq(result), 1)
  def valid[R]                                        = Checked[R](true, Seq(), 1)
  def valid[R](result: R)                             = Checked[R](true, Seq(result), 1)
  def invalid[R]                                      = Checked[R](false, Seq(), 1)
  def invalid[R](result: R)                           = Checked[R](false, Seq(result), 1)
  def merge[R](checked: Seq[Checked[R]]): Checked[R] = {
    val valid           = checked.forall(_.valid)
    val results: Seq[R] = checked.flatMap(_.results)
    val ignoredKeywords = checked.flatMap(_.ignoredKeywords)
    Checked(valid, results, 1 + count(checked), ignoredKeywords)
  }
  def count[R](checked: Seq[Checked[R]]): Int = checked.map(_.count).sum
}

case class Checks(
    schema: SchemaValue,
    checks: Seq[Check] = Seq.empty[Check],
    ignoredKeywords: Set[String] = Set.empty
) {
  import SeqUtil._

  private def withCheck(check: Check): Checks                         = this.copy(checks = checks :+ check)
  private def withChecks(checks: Checks)(f: Checks => Checks): Checks = f(checks).withIgnored(checks.ignoredKeywords)
  private def withChecks(checks: Map[String, Checks])(f: Map[String, Checks] => Check): Checks =
    withCheck(f(checks)).withIgnored(checks.values.flatMap(_.ignoredKeywords).toSet)
  private def withChecks(checks: Seq[Checks])(f: Seq[Checks] => Check): Checks =
    withCheck(f(checks)).withIgnored(checks.flatMap(_.ignoredKeywords).toSet)
  private def withChecks(
      values: Seq[Value],
      scope: DynamicScope
  )(f: Seq[Checks] => Check)(implicit resolver: SchemaResolver): Either[SchemaErrors, Checks] = {
    val checks0 = values
      .map(v => Checks.parseKeywords(SchemaValue(v), scope))
      .toSeq
    for {
      checks <- sequenceAllLefts(checks0)
    } yield {
      withChecks(checks)(f)
    }
  }

  type SchemaErrors = Checks.SchemaErrors

  def withKeyword(keyword: String, value: Value, scope: DynamicScope)(implicit
      resolver: SchemaResolver
  ): Either[SchemaErrors, Checks] = {
    val scope1 = scope.push(keyword)
    (keyword, value) match {
      // TODO validation vocabulary
      case ("type", StringValue(typeName)) =>
        Right(
          getTypeCheck(typeName)
            .map(withCheck(_))
            .getOrElse(withIgnored(s"${keyword}-${typeName}"))
        )

      // TODO validation vocabulary
      case ("type", ArrayValue(values)) => {
        def typeNames = Value.asStrings(values)
        Right(withCheck(UnionTypeCheck(typeNames.flatMap(getTypeCheck(_)))))
      }

      case ("not", value) =>
        for {
          checks <- Checks.parseKeywords(SchemaValue(value), scope1)
        } yield {
          withChecks(checks)(c => withCheck(NotCheck(c)))
        }

      case ("items", value) =>
        for {
          checks <- Checks.parseKeywords(SchemaValue(value), scope1)
        } yield {
          withChecks(checks) { checks =>
            updateCheck(ArrayItemsCheck())(check => check.copy(items = Some(checks)))
          }
        }

      case ("prefixItems", ArrayValue(vs)) =>
        val checks0 = vs.map(v => Checks.parseKeywords(SchemaValue(v), scope1))
        for {
          checks <- sequenceAllLefts(checks0)
        } yield {
          updateCheck(ArrayItemsCheck())(check => check.copy(prefixItems = checks))
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
          withChecks(checks) { checks =>
            updateCheck(ObjectPropertiesCheck())(check => check.copy(additionalProperties = Some(checks)))
          }
        }

      case ("unevaluatedProperties", value) =>
        // TODO needs to be implemented against "annotation results"
        // for now mapping on additionalProperties ...
        for {
          checks <- Checks.parseKeywords(SchemaValue(value), scope1)
        } yield {
          withChecks(checks) { checks =>
            updateCheck(ObjectPropertiesCheck())(check => check.copy(additionalProperties = Some(checks)))
          }
        }

      case ("required", ArrayValue(values)) => {
        def names = Value.asStrings(values)
        Right(withCheck(ObjectRequiredCheck(names)))
      }

      case ("allOf", ArrayValue(values)) => {
        withChecks(values, scope1)(AllOfCheck)
      }

      case ("anyOf", ArrayValue(values)) => {
        withChecks(values, scope1)(AnyOfCheck)
      }

      case ("oneOf", ArrayValue(values)) => {
        withChecks(values, scope1)(OneOfCheck)
      }

      case ("if", value) =>
        updateChecks(SchemaValue(value), scope1)(IfThenElseCheck()) { (checks, check) =>
          check.copy(ifChecks = Some(checks))
        }

      case ("then", value) =>
        updateChecks(SchemaValue(value), scope1)(IfThenElseCheck()) { (checks, check) =>
          check.copy(thenChecks = Some(checks))
        }

      case ("else", value) =>
        updateChecks(SchemaValue(value), scope1)(IfThenElseCheck()) { (checks, check) =>
          check.copy(elseChecks = Some(checks))
        }

      // TODO validation vocabulary
      case ("enum", ArrayValue(values)) => {
        Right(withCheck(EnumCheck(values)))
      }

      // TODO validation vocabulary
      case ("const", value) => {
        Right(withCheck(EnumCheck(Seq(value))))
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
            .getOrElse(Left(Seq(SchemaError(s"""missing reference "${ref}""""))))
          check = lazyResolveCheck(resolution, scope1)
        } yield {
          withCheck(check)
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
          withCheck(check)
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
        Right(withCheck(PatternCheck(pattern)))
      }

      // TODO validation vocabulary
      case ("format", StringValue(format)) => {
        Right(withCheck(FormatCheck(format)))
      }

      // TODO validation vocabulary
      case ("minimum", NumberValue(v)) => {
        Right(withCheck(MinimumCheck(v)))
      }

      // TODO validation vocabulary
      case ("exclusiveMinimum", NumberValue(v)) => {
        Right(withCheck(MinimumCheck(v, true)))
      }

      // TODO validation vocabulary
      case ("minItems", NumberValue(v)) if v >= 0 => {
        Right(withCheck(MinItemsCheck(v)))
      }

      // TODO validation vocabulary
      case ("uniqueItems", BoolValue(v)) => {
        Right(withCheck(UniqueItemsCheck(v)))
      }

      case ("propertyNames", value) =>
        for {
          checks <- Checks.parseKeywords(SchemaValue(value), scope1)
        } yield {
          withChecks(checks)(c => withCheck(PropertyNamesCheck(c)))
        }

      // TODO validation vocabulary
      case ("multipleOf", NumberValue(v)) if v > 0 => {
        Right(withCheck(MultipleOfCheck(v)))
      }

      // TODO validation vocabulary
      case ("maximum", NumberValue(v)) => {
        Right(withCheck(MaximumCheck(v)))
      }

      // TODO validation vocabulary
      case ("exclusiveMaximum", NumberValue(v)) => {
        Right(withCheck(MaximumCheck(v, true)))
      }

      // TODO validation vocabulary
      case ("maxLength", NumberValue(v)) if v >= 0 => {
        Right(withCheck(MaxLengthCheck(v)))
      }

      // TODO validation vocabulary
      case ("minLength", NumberValue(v)) if v >= 0 => {
        Right(withCheck(MinLengthCheck(v)))
      }

      // TODO validation vocabulary
      case ("maxItems", NumberValue(v)) if v >= 0 => {
        Right(withCheck(MaxItemsCheck(v)))
      }

      // TODO validation vocabulary
      case ("maxProperties", NumberValue(v)) if v >= 0 => {
        Right(withCheck(MaxPropertiesCheck(v)))
      }

      // TODO validation vocabulary
      case ("minProperties", NumberValue(v)) if v >= 0 => {
        Right(withCheck(MinPropertiesCheck(v)))
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
        Right(withCheck(DependentRequiredCheck(vv)))
      }

      case ("dependentSchemas", ObjectValue(v)) => {
        val checks0 = v.view.map { case (p, v) =>
          Checks
            .parseKeywords(SchemaValue(v), scope1)
            .map(p -> _)
        }.toSeq
        for {
          checks1 <- sequenceAllLefts(checks0).map(_.toMap)
        } yield {
          withCheck(DependentSchemasCheck(checks1))
        }
      }

      case ("contains", v) =>
        for {
          checks <- Checks.parseKeywords(SchemaValue(v), scope1)
        } yield {
          withChecks(checks) { checks =>
            updateCheck(ContainsCheck())(check => check.copy(schema = Some(checks)))
          }
        }

      // TODO validation vocabulary
      case ("minContains", NumberValue(v)) if v >= 0 =>
        Right(updateCheck(ContainsCheck())(check => check.copy(min = Some(v.toInt))))

      // TODO validation vocabulary
      case ("maxContains", NumberValue(v)) if v >= 0 =>
        Right(updateCheck(ContainsCheck())(check => check.copy(max = Some(v.toInt))))

      case _ => Right(withIgnored(keyword))
    }
  }

  private def lazyResolveCheck(resolution: SchemaResolver.Resolution, scope: DynamicScope)(implicit
      resolver: SchemaResolver
  ): LazyResolveCheck = {
    val resolveLater = { () =>
      val schema    = resolution._1
      val resolver1 = resolution._2
      Checks.parseKeywords(schema, scope)(resolver1)
    }
    LazyResolveCheck(resolveLater)
  }

  private def mapChecksFor(
      props: Map[String, Value],
      scope: DynamicScope
  )(f: Map[String, Checks] => Checks)(implicit resolver: SchemaResolver) = {
    val propChecks = props.view
      .mapValues(v => Checks.parseKeywords(SchemaValue(v), scope))
      .map {
        case (prop, Right(checks)) => Right((prop, checks))
        case (prop, Left(errors))  => Left(errors.map(_.prefix(Pointer.empty / prop)))
      }
      .toSeq
    for {
      propChecks1 <- sequenceFirstLeft(propChecks)
      checks1 = Map.from(propChecks1)
      ignored = checks1.values.flatMap(_.ignoredKeywords).toSet
    } yield {
      f(checks1).withIgnored(ignored)
    }
  }

  private def updateCheck[C <: Check: ClassTag](newCheck: => C)(f: C => C): Checks = {
    val checks1: Seq[Check] =
      if (
        checks.exists {
          case check: C => true
          case other    => false
        }
      ) {
        checks
      } else {
        checks :+ newCheck
      }
    this.copy(checks = checks1.map {
      case check: C => f(check)
      case other    => other
    })
  }

  private def updateChecks[C <: Check: ClassTag](
      schema: SchemaValue,
      scope: DynamicScope
  )(newCheck: => C)(f: (Checks, C) => C)(implicit resolver: SchemaResolver): Either[SchemaErrors, Checks] = {
    for {
      checks <- Checks.parseKeywords(schema, scope)
    } yield {
      withChecks(checks) { checks =>
        updateCheck(newCheck)(f(checks, _))
      }
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
  type SchemaErrors = Seq[SchemaError]

  def parseKeywords(schema: SchemaValue, scope: DynamicScope)(implicit
      resolver: SchemaResolver
  ): Either[SchemaErrors, Checks] = {
    val scope1 = SchemaValue
      .id(schema)
      .map(id => scope.push(resolver.absolute(id)))
      .getOrElse(scope)
    val resolver1 = SchemaValue
      .id(schema)
      .map(id => resolver.withBase(resolver.absolute(id)))
      .getOrElse(resolver)
    schema.value match {
      case BoolValue(v) => Right(Checks(schema).withCheck(TrivialCheck(v)))
      case ObjectValue(keywords) =>
        if (keywords.isEmpty) {
          Right(Checks(schema).withCheck(TrivialCheck(true)))
        } else {
          keywords
            .foldLeft[Either[SchemaErrors, Checks]](Right(Checks(schema))) { case (checks, (keyword, value)) =>
              val prefix = Pointer.empty / keyword
              checks
                .flatMap(_.withKeyword(keyword, value, scope1)(resolver1))
                .swap
                .map(_.map(_.prefix(prefix)))
                .swap
            }
        }
      case _ => Left(Seq(SchemaError(s"invalid schema ${schema}")))
    }
  }

}
