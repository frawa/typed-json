package frawa.typedjson.eval

import frawa.typedjson.keywords.Keywords
import Keywords.KeywordWithLocation
import frawa.typedjson.keywords.*
import frawa.typedjson.parser.Value
import frawa.typedjson.parser.Value.*
import java.util.regex.Pattern
import scala.util.Try
import java.net.URI
import java.util.UUID
import frawa.typedjson.pointer.Pointer

/*
class ProcDefault[O: OutputOps] extends Proc[O]:
  import frawa.typedjson.validation.*

  private type EvalFun[O] = Value => O

  def process(keyword: KeywordWithLocation): EvalFun[O] =
    keyword.value match {
      case assertion: AssertionKeyword   => validate(assertion)
      case aggregator: ApplicatorKeyword => aggregate(aggregator)
    }

  private def validate(keyword: AssertionKeyword): EvalFun[O] =
    keyword match {
      case NullTypeKeyword                 => validateType(nullTypeMismatch)
      case BooleanTypeKeyword              => validateType(booleanTypeMismatch)
      case StringTypeKeyword               => validateType(stringTypeMismatch)
      case NumberTypeKeyword               => validateType(numberTypeMismatch)
      case IntegerTypeKeyword              => validateInteger()
      case ArrayTypeKeyword                => validateType(arrayTypeMismatch)
      case ObjectTypeKeyword               => validateType(objectTypeMismatch)
      case ObjectRequiredKeyword(required) => validateObjectRequired(required)
      case TrivialKeyword(valid)           => validateTrivial(valid)
      case EnumKeyword(values)             => validateEnum(values)
      case PatternKeyword(pattern)         => validatePattern(pattern)
      case FormatKeyword(format)           => validateFormat(format)
      case MinimumKeyword(v, exclude)      => validateMinimum(v, exclude)
      case UniqueItemsKeyword(v)           => validateUniqueItems(v)
      case MultipleOfKeyword(n)            => validateMultipleOf(n)
      case MaximumKeyword(v, exclude)      => validateMaximum(v, exclude)
      case MaxLengthKeyword(v)             => validateMaxLength(v)
      case MinLengthKeyword(v)             => validateMinLength(v)
      case MaxItemsKeyword(v)              => validateMaxItems(v)
      case MinItemsKeyword(v)              => validateMinItems(v)
      case MaxPropertiesKeyword(v)         => validateMaxProperties(v)
      case MinPropertiesKeyword(v)         => validateMinProperties(v)
      case DependentRequiredKeyword(v)     => validateDependentRequired(v)
    }

  private def aggregate(
      aggregator: ApplicatorKeyword
  )(using ResultOps[R])(using ops: OutputOps[O]): EvalFun[O, R] = aggregator match {
    case c: ArrayItemsKeyword            => evalArrayItems(c)
    case c: ObjectPropertiesKeyword      => evalObjectProperties(c)
    case c: NotKeyword                   => evalNot(c.keywords)
    case c: AllOfKeyword                 => evalApplicator(c.keywords)(ops.all)
    case c: AnyOfKeyword                 => evalApplicator(c.keywords)(ops.any)
    case c: OneOfKeyword                 => evalApplicator(c.keywords)(ops.one)
    case c: UnionTypeKeyword             => evalUnionType(c)
    case c: IfThenElseKeyword            => evalIfThenElse(c)
    case c: PropertyNamesKeyword         => evalPropertyNames(c)
    case c: LazyParseKeywords            => evalLazyParseKeywords(c)
    case c: DependentSchemasKeyword      => evalDependentSchemas(c)
    case c: ContainsKeyword              => evalContains(c)
    case c: UnevaluatedItemsKeyword      => ??? // evalUnevaluated(processing, c)
    case c: UnevaluatedPropertiesKeyword => ??? // evalUnevaluated(processing, c)
  }

  private val nullTypeMismatch    = TypeMismatch[NullValue.type]("null")
  private val booleanTypeMismatch = TypeMismatch[BoolValue]("boolean")
  private val stringTypeMismatch  = TypeMismatch[StringValue]("string")
  private val numberTypeMismatch  = TypeMismatch[NumberValue]("number")
  private val arrayTypeMismatch   = TypeMismatch[ArrayValue]("array")
  private val objectTypeMismatch  = TypeMismatch[ObjectValue]("object")

  private def validateType[T <: Value](error: TypeMismatch[T])(using ops: OutputOps[O]): Fun = value =>
    value.value match
      case _: T => ops.valid
      case _    => ops.invalid(error, value.pointer)

  private def validateInteger()(using ops: OutputOps[O]): Fun =
    lazy val error = TypeMismatch[NumberValue]("integer")
    value =>
      value.value match
        case NumberValue(v) =>
          if v.isWhole then ops.valid
          else ops.invalid(error, value.pointer)
        case _ => ops.invalid(error, value.pointer)

  private def validateObjectRequired(required: Seq[String])(using ops: OutputOps[O]): Fun = value =>
    value.value match
      case ObjectValue(propertiesValues) =>
        val missingNames = required.filter(!propertiesValues.contains(_))
        if missingNames.isEmpty then ops.valid
        else ops.invalid(MissingRequiredProperties(missingNames), value.pointer)
      case _ => ops.valid

  private def validateTrivial(valid: Boolean)(using ops: OutputOps[O]): Fun = { value =>
    if valid then ops.valid
    else ops.invalid(FalseSchemaReason(), value.pointer)
  }

  private def validateEnum(values: Seq[Value])(using ops: OutputOps[O]): Fun = { value =>
    if values.contains(value.value) then ops.valid
    else ops.invalid(NotInEnum(values), value.pointer)
  }

  private def validatePattern(pattern: String)(using ops: OutputOps[O]): Fun =
    val r = pattern.r
    value =>
      value.value match
        case StringValue(v) =>
          if r.findFirstIn(v).isDefined then ops.valid
          else ops.invalid(PatternMismatch(pattern), value.pointer)
        case _ => ops.valid

  private def validateFormat(format: String)(using ops: OutputOps[O]): Fun =
    format match
      case "regex" =>
        validateStringValue(FormatMismatch(format)) { v =>
          Try(Pattern.compile(v)).isSuccess
        }
      case "email" =>
        validateStringValue(FormatMismatch(format)) { v =>
          // see https://emailregex.com/
          val regex =
            """(?:[a-z\\d!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z\\d!#$%&'*+/=?^_`{|}~-]+)*|"(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\x5b\x5d-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])*")@(?:(?:[a-z\\d](?:[a-z\\d-]*[a-z\\d])?\.)+[a-z\\d](?:[a-z\\d-]*[a-z\\d])?|\[(?:(?:25[0-5]|2[0-4][\\d]|[01]?[\\d][\\d]?)\.){3}(?:25[0-5]|2[0-4][\\d]|[01]?[\\d][\\d]?|[a-z\\d-]*[a-z\\d]:(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])+)\])""".r
          regex.matches(v)
        }
      case "idn-email" =>
        validateStringValue(FormatMismatch(format)) { v =>
          // see https://stackoverflow.com/questions/13992403/regex-validation-of-email-addresses-according-to-rfc5321-rfc5322
          val regex =
            """([!#-'*+/-9=?A-Z^-~-]+(\.[!#-'*+/-9=?A-Z^-~-]+)*|"([ ]!#-[^-~ \t]|(\\[\t -~]))+")@([!#-'*+/-9=?A-Z^-~-]+(\.[!#-'*+/-9=?A-Z^-~-]+)*|\[[\t -Z^-~]*])""".r
          regex.matches(v)
        }
      case "hostname" =>
        validateStringValue(FormatMismatch(format)) { v =>
          // see https://stackoverflow.com/questions/106179/regular-expression-to-match-dns-hostname-or-ip-address
          val regex =
            """^(([a-zA-Z\\d]|[a-zA-Z\\d][a-zA-Z\\d\-]*[a-zA-Z\\d])\.)*([A-Za-z\\d]|[A-Za-z\\d][A-Za-z\\d\-]*[A-Za-z\\d])$""".r
          regex.matches(v)
        }
      case "idn-hostname" =>
        validateStringValue(FormatMismatch(format)) { v =>
          // see https://stackoverflow.com/questions/11809631/fully-qualified-domain-name-validation/26618995
          val regex =
            """(?=^.{4,253}$)(^((?!-)[a-zA-Z\\d-]{1,63}(?<!-)\.)+[a-zA-Z]{2,63}$)""".r
          regex.matches(v)
        }
      case "ipv4" =>
        // see https://stackoverflow.com/questions/106179/regular-expression-to-match-dns-hostname-or-ip-address
        validateStringValue(FormatMismatch(format)) { v =>
          val regex =
            """^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$""".r
          regex.matches(v)
        }
      case "ipv6" =>
        // see https://stackoverflow.com/questions/53497/regular-expression-that-matches-valid-ipv6-addresses
        validateStringValue(FormatMismatch(format)) { v =>
          val regex =
            """(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))""".r
          regex.matches(v)
        }
      case "uri" =>
        validateStringValue(FormatMismatch(format)) { v =>
          Try(new URI(v)).map(_.isAbsolute).getOrElse(false)
        }
      case "uri-reference" =>
        // TODO reference?
        validateStringValue(FormatMismatch(format)) { v =>
          Try(new URI(v)).isSuccess
        }
      case "uri-template" =>
        // see https://datatracker.ietf.org/doc/html/rfc6570
        // TODO template?
        // TODO may use for iri?
        validateStringValue(FormatMismatch(format)) { v =>
          Try(new URI(v)).isSuccess
        }
      case "iri" =>
        // see https://datatracker.ietf.org/doc/html/rfc3987
        // TODO iri
        validateStringValue(FormatMismatch(format)) { v =>
          Try(new URI(v)).map(_.isAbsolute).getOrElse(false)
        }
      case "iri-reference" =>
        // see https://datatracker.ietf.org/doc/html/rfc3987
        // TODO iri
        // TODO reference?
        validateStringValue(FormatMismatch(format)) { v =>
          Try(new URI(v)).isSuccess
        }
      case "uuid" =>
        // see https://datatracker.ietf.org/doc/html/rfc4122
        validateStringValue(FormatMismatch(format)) { v =>
          Try(UUID.fromString(v)).isSuccess
        }
      case "json-pointer" =>
        validateStringValue(FormatMismatch(format)) { v =>
          Pointer.parse(v).toString().equals(v)
        }
      case "relative-json-pointer" =>
        validateStringValue(FormatMismatch(format)) { v =>
          !v.isBlank() && !v.startsWith("/") && Pointer.parse("/" + v).toString().equals("/" + v)
        }
      case "date-time" =>
        // https://datatracker.ietf.org/doc/html/rfc3339
        validateStringValue(FormatMismatch(format)) { v =>
          val regex_date = "\\d{4}-\\d{2}-\\d{2}"
          val regex_time = "\\d{2}:\\d{2}:\\d{2}(\\.\\d+)?(Z|([+\\-])\\d{2}:\\d{2})"
          val date_time  = s"${regex_date}T$regex_time"
          date_time.r.matches(v)
        }
      case "date" =>
        // https://datatracker.ietf.org/doc/html/rfc3339
        validateStringValue(FormatMismatch(format)) { v =>
          val `regex-date` =
            "\\d{4}-\\d{2}-\\d{2}".r
          `regex-date`.matches(v)
        }
      case "time" =>
        // https://datatracker.ietf.org/doc/html/rfc3339
        validateStringValue(FormatMismatch(format)) { v =>
          val `regex-time` =
            "(\\d{2}):(\\d{2}):(\\d{2})(\\.\\d+)?(Z|([+\\-]\\d{2}:\\d{2}))".r
          `regex-time`
            .findFirstMatchIn(v)
            .exists { m =>
              val hour   = m.group(1).toInt
              val minute = m.group(2).toInt
              val second = m.group(3).toInt
              Range.inclusive(0, 23).contains(hour) &&
              Range.inclusive(0, 59).contains(minute) &&
              Range.inclusive(0, 59).contains(second)
            }
//          Try {
////            val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss[.SSS]XXX")
//            val formatter = DateTimeFormatter
//              .ofPattern("HH:mm:ss[.SSS]XXX")
//              .withResolverStyle(ResolverStyle.STRICT);
//            OffsetTime.parse(v, formatter)
//          }.recover { ex =>
//            println("FW ?", ex)
//            throw ex
//          }.isSuccess
        }
      case "duration" =>
        // https://datatracker.ietf.org/doc/html/rfc3339
        // TODO test me
        validateStringValue(FormatMismatch(format)) { v =>
          val `dur-second` = "\\d+S"
          val `dur-minute` = s"\\d+M(${`dur-second`})?"
          val `dur-hour`   = s"\\d+H(${`dur-minute`})?"
          val `dur-day`    = "\\d+D"
          val `dur-time`   = s"T(${`dur-hour`}|${`dur-minute`}|${`dur-second`})"
          val `dur-week`   = "\\d+W"
          val `dur-month`  = s"\\d+M(${`dur-day`})?"
          val `dur-year`   = s"\\d+Y(${`dur-month`})?"
          val `dur-date`   = s"(${`dur-day`}|${`dur-month`}|${`dur-year`})(${`dur-time`})?"
          val duration     = s"P(${`dur-date`}|${`dur-time`}|${`dur-week`})"
          duration.r.matches(v)
        }
      case _ => value => ops.valid(UnknownFormat(format), value.pointer)

  private def validateStringValue(
      error: => ValidationError
  )(validate: String => Boolean)(using ops: OutputOps[O]): Fun = { value =>
    value.value match
      case StringValue(v) =>
        if validate(v) then ops.valid
        else ops.invalid(error, value.pointer)
      case _ => ops.valid
  }

  private def validateMinimum(min: BigDecimal, exclude: Boolean)(using ops: OutputOps[O]): Fun =
    validateNumberValue(MinimumMismatch(min, exclude)) { v =>
      if exclude then min < v else min <= v
    }

  private def validateNumberValue(
      error: => ValidationError
  )(validate: BigDecimal => Boolean)(using ops: OutputOps[O]): Fun = { value =>
    value.value match
      case NumberValue(v) =>
        if validate(v) then ops.valid
        else ops.invalid(error, value.pointer)
      case _ => ops.valid
  }

  private def validateArrayValue(
      error: => ValidationError
  )(validate: Seq[Value] => Boolean)(using ops: OutputOps[O]): Fun = { value =>
    value.value match
      case ArrayValue(v) =>
        if validate(v) then ops.valid
        else ops.invalid(error, value.pointer)
      case _ => ops.valid
  }

  private def validateUniqueItems(unique: Boolean)(using ops: OutputOps[O]): Fun =
    validateArrayValue(ItemsNotUnique()) { v =>
      !unique || v.distinct.length == v.length
    }

  private def validateMultipleOf(n: BigDecimal)(using ops: OutputOps[O]): Fun =
    validateNumberValue(NotMultipleOf(n)) { v =>
      (v / n).isValidInt
    }

  private def validateMaximum(max: BigDecimal, exclude: Boolean)(using ops: OutputOps[O]): Fun =
    validateNumberValue(MaximumMismatch(max, exclude)) { v =>
      if exclude then max > v else max >= v
    }

  private def validateMaxLength(max: BigDecimal)(using ops: OutputOps[O]): Fun =
    validateStringValue(MaxLengthMismatch(max)) { v =>
      countCharPoints(v) <= max
    }

  private def validateMinLength(min: BigDecimal)(using ops: OutputOps[O]): Fun =
    validateStringValue(MinLengthMismatch(min)) { v =>
      countCharPoints(v) >= min
    }

  private def countCharPoints(text: String): Int =
    var i     = 0
    var count = 0
    while i < text.length() do
      i += Character.charCount(text.codePointAt(i))
      count += 1
    count

  private def validateMaxItems(max: BigDecimal)(using ops: OutputOps[O]): Fun =
    validateArrayValue(MaxItemsMismatch(max)) { v =>
      max >= v.length
    }

  private def validateMinItems(min: BigDecimal)(using ops: OutputOps[O]): Fun =
    validateArrayValue(MinItemsMismatch(min)) { v =>
      min <= v.length
    }

  private def validateObjectValue(
      error: => ValidationError
  )(validate: Map[String, Value] => Boolean)(using ops: OutputOps[O]): Fun =
    value =>
      value.value match
        case ObjectValue(v) =>
          if validate(v) then ops.valid
          else ops.invalid(error, value.pointer)
        case _ => ops.valid

  private def validateMaxProperties(max: BigDecimal)(using ops: OutputOps[O]): Fun =
    validateObjectValue(MaxPropertiesMismatch(max)) { v =>
      max >= v.keySet.size
    }

  private def validateMinProperties(min: BigDecimal)(using ops: OutputOps[O]): Fun =
    validateObjectValue(MinPropertiesMismatch(min)) { v =>
      min <= v.keySet.size
    }

  private def validateDependentRequired(required: Map[String, Seq[String]])(using ops: OutputOps[O]): Fun = { value =>
    value.value match
      case ObjectValue(v) =>
        val missing = required
          .flatMap { case (property, required) =>
            v
              .get(property)
              .map { _ => required.filterNot(v.contains) }
              .filterNot(_.isEmpty)
              .map(property -> _)
          }
        if missing.isEmpty then ops.valid
        else ops.invalid(DependentRequiredMissing(missing), value.pointer)
      case _ => ops.valid
  }

  private def evalArrayItems(keyword: ArrayItemsKeyword)(using rops: ResultOps[R])(using
      ops: OutputOps[O]
  ): EvalFun[O, R] =
    val pPrefix = keyword.prefixItems.map(process(_))
    val pItems  = keyword.items.map(process(_))
    applyToArray(pPrefix, pItems)(ops.all)

  private def applyToArray(pPrefix: Seq[EvalFun[O, R]], pItems: Option[EvalFun[O, R]])(
      aggregate: Seq[O] => O
  )(using rops: ResultOps[R])(using ops: OutputOps[O]): EvalFun[O, R] = (value: InnerValue) =>
    value.value match
      case ArrayValue(vs) =>
        val indexed = vs.zipWithIndex
          .map { case (v, index) =>
            InnerValue(v, value.pointer / index)
          }
        val resultPrefix = pPrefix.zip(indexed).map { case (p, v) => p(v) }
        val result       = pItems.map(pItems => indexed.drop(pPrefix.size).map(pItems)).getOrElse(Seq())
        val indices      = Seq.range(0, resultPrefix.size + result.size)
        // aggregateAll(aggregate, resultPrefix ++ result, value).add(WithPointer(EvaluatedIndices(indices)))
        Util.sequence(resultPrefix ++ result).map(aggregate)
      case _ => rops.pure(ops.valid)

  private def evalNot(keywords: Keywords)(using ResultOps[R])(using ops: OutputOps[O]): EvalFun[O, R] = value =>
    val p      = process(keywords)
    val result = p(value)
    result.map(_.not)

  private def evalApplicator(keywords: Seq[Keywords])(
      aggregate: Seq[O] => O
  )(using ResultOps[R])(using OutputOps[O]): EvalFun[O, R] =
    val p = keywords.map(process(_))
    applyToValue(p)(aggregate)

  private def applyToValue(ps: Seq[EvalFun[O, R]])(aggregate: Seq[O] => O)(using ResultOps[R]): EvalFun[O, R] = value =>
    val result = ps.map(_(value))
    Util.sequence(result).map(aggregate)

  private def aggregateThem(rs: Seq[R[O]], aggregate: Seq[O] => O)(using ResultOps[R]): R[O] =
    Util.sequence(rs).map(aggregate)

  private def evalObjectProperties(
      keyword: ObjectPropertiesKeyword
  )(using ResultOps[R])(using ops: OutputOps[O]): EvalFun[O, R] =
    val psProperties = keyword.properties.map { case (key, keywords) =>
      val partial: PartialFunction[String, () => EvalFun[O, R]] =
        case k if k == key =>
          () => process(keywords)
      partial
    }.toSeq

    val psPatterns = keyword.patternProperties.map { case (regex, keywords) =>
      val r = regex.r
      val partial: PartialFunction[String, () => EvalFun[O, R]] =
        case k if r.findFirstIn(k).isDefined => () => process(keywords)
      partial
    }.toSeq

    val psBoth = psProperties ++ psPatterns

    val psAll: PartialFunction[String, () => EvalFun[O, R]] =
      case k if psBoth.exists(_.isDefinedAt(k)) =>
        () => all(psBoth.map(_.lift).flatMap { p => p(k).map(_()) })

    val psAdditional = keyword.additionalProperties.map { keywords =>
      val partial: PartialFunction[String, () => EvalFun[O, R]] = { _ => () => process(keywords) }
      partial
    }

    val ps = psAdditional.map(psAll.orElse(_)).getOrElse(psAll)
    value => {
      applyToObject(ps)(ops.all)(value)
    }

  private def applyToObject(
      ps: => PartialFunction[String, () => EvalFun[O, R]]
  )(aggregate: Seq[O] => O)(using rops: ResultOps[R])(using ops: OutputOps[O]): EvalFun[O, R] = value =>
    value.value match
      case ObjectValue(vs) =>
        val evaluated = vs.view
          .map { case (key, v) =>
            (key, InnerValue(v, value.pointer / key))
          }
          .flatMap { case (key, inner) =>
            val p = ps.lift(key)
            p.map(p => (key, p()(inner)))
          }
          .toSeq
        val result = evaluated.map(_._2)
        // val evaluatedKeys = EvaluatedProperties(evaluated.filter(_._2.isValid).map(_._1).toSet)
        // val annotation    = WithPointer(evaluatedKeys, value.pointer)
        // aggregateAll(aggregate, result, value).add(annotation)
        aggregateThem(result, aggregate)
      case _ => rops.pure(ops.valid)

  private def evalUnionType(keyword: UnionTypeKeyword)(using ResultOps[R])(using
      ops: OutputOps[O]
  ): EvalFun[O, R] =
    val p = keyword.keywords.map(process(_))
    applyToValue(p)(ops.one)

  private def evalIfThenElse(keyword: IfThenElseKeyword)(using ResultOps[R])(using OutputOps[O]): EvalFun[O, R] =
    keyword.ifKeywords
      .map(ifChecks =>
        applyCondition(
          process(ifChecks),
          option(keyword.thenKeywords.map(process(_))),
          option(keyword.elseKeywords.map(process(_)))
        )
      )
      .getOrElse(noop)

  private def applyCondition(pIf: EvalFun[O, R], pThen: EvalFun[O, R], pElse: EvalFun[O, R])(using ResultOps[R])(using
      ops: OutputOps[O]
  ): EvalFun[O, R] = value =>
    val ifResult = pIf(value)
    // val result   = if ifResult.valid then Seq(ifResult, pThen(value)) else Seq(pElse(value))
    ifResult
      .flatMap { o =>
        if o.isValid then Util.sequence(Seq(ifResult, pThen(value)))
        else Util.sequence(Seq(pElse(value)))
      }
      .map(ops.all)

  private def noop(using rops: ResultOps[R])(using ops: OutputOps[O]): EvalFun[O, R] = _ => rops.pure(ops.valid)

  private def option(p: Option[EvalFun[O, R]])(using ResultOps[R])(using OutputOps[O]): EvalFun[O, R] =
    p.getOrElse(noop)

  private def evalPropertyNames(
      keyword: PropertyNamesKeyword
  )(using rops: ResultOps[R])(using ops: OutputOps[O]): EvalFun[O, R] = value =>
    value.value match
      case ObjectValue(vs) =>
        val p     = process(keyword.keywords)
        val names = vs.keySet
        val result = names.map { name =>
          (name, p(InnerValue(StringValue(name), value.pointer / name)))
        }.toSeq
        // val validNames = result.filter(_._2.isValid).map(_._1).toSet
        // val annotation = WithPointer(EvaluatedProperties(validNames), value.pointer)
        // aggregateAll(aggregate, result.map(_._2), value).add(annotation)
        aggregateThem(result.map(_._2), ops.all)
      case _ => rops.pure(ops.valid)

  private def evalLazyParseKeywords(keyword: LazyParseKeywords)(using rops: ResultOps[R])(using
      ops: OutputOps[O]
  ): EvalFun[O, R] =
    keyword.parse() match
      case Right(keywords) => process(keywords)
      case Left(problems)  => _ => rops.pure(ops.invalid(problems))

  private def evalDependentSchemas(keyword: DependentSchemasKeyword)(using rops: ResultOps[R])(using
      ops: OutputOps[O]
  ): EvalFun[O, R] =
    value =>
      value.value match
        case ObjectValue(v) =>
          val ps     = v.keySet.flatMap(keyword.keywords.get).map(process(_)).toSeq
          val result = ps.map(_(value))
          aggregateThem(result, ops.all)
        case _ => rops.pure(ops.valid)

  private def evalContains(keyword: ContainsKeyword)(using rops: ResultOps[R])(using
      ops: OutputOps[O]
  ): EvalFun[O, R] = value =>
    value.value match
      case ArrayValue(vs) =>
        keyword.schema
          .map { schema =>
            val p = process(schema)
            val indexed = vs.zipWithIndex
              .map { case (v, index) =>
                InnerValue(v, value.pointer / index)
              }
            val result = indexed.map(p(_))
            // val validIndices = result.zipWithIndex.filter(_._1.isValid).map(_._2)
            // aggregateAll(aggregate, result, value).add(
            //   typedjson.keywords.WithPointer(EvaluatedIndices(validIndices), value.pointer)
            // )
            aggregateThem(result, os => ops.contains(os, keyword.min, keyword.max, value.pointer))
          }
          .getOrElse(rops.pure(ops.valid))
      case _ => rops.pure(ops.valid)
 */
