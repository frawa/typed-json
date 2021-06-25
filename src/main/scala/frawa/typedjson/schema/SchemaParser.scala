package frawa.typedjson.schema

import frawa.typedjson.parser.Value
import frawa.typedjson.parser.NullValue
import frawa.typedjson.parser.ObjectValue
import frawa.typedjson.parser.Parser
import frawa.typedjson.parser.StringValue
import frawa.typedjson.parser.ArrayValue
import frawa.typedjson.parser.BoolValue

trait Schema
case object NullSchema                                                       extends Schema
case object TrueSchema                                                       extends Schema
case object FalseSchema                                                      extends Schema
case object BooleanSchema                                                    extends Schema
case object StringSchema                                                     extends Schema
case object NumberSchema                                                     extends Schema
case class ArraySchema(items: Schema)                                        extends Schema
case class ObjectSchema(properties: Map[String, Schema])                     extends Schema
case class RootSchema(id: String, schema: Schema, defs: Map[String, Schema]) extends Schema
case class RefSchema(ref: String)                                            extends Schema
case class SchemaWithApplicators(
    schema: Schema,
    allOf: Seq[Schema],
    anyOf: Seq[Schema],
    oneOf: Seq[Schema],
    notOp: Option[Schema]
) extends Schema

trait SchemaParser {
  def parseRoot(json: String)(implicit parser: Parser): Either[String, RootSchema]
  def parseSchema(json: String)(implicit parser: Parser): Either[String, Schema]
}

object SchemaParser {
  def apply(json: String)(implicit parser: Parser, schemaParser: SchemaParser): Either[String, RootSchema] =
    schemaParser.parseRoot(json)
  def schema(json: String)(implicit parser: Parser, schemaParser: SchemaParser): Either[String, Schema] =
    schemaParser.parseSchema(json)
}

object SchemaValueDecoder extends SchemaParser {
  import Decoding._

  override def parseRoot(json: String)(implicit parser: Parser): Either[String, RootSchema] = {
    for {
      value  <- parser.parse(json)
      schema <- rootSchema.decode(value)
    } yield schema
  }

  override def parseSchema(json: String)(implicit parser: Parser): Either[String, Schema] = {
    for {
      value <- parser.parse(json)
      s     <- schema.decode(value)
    } yield s
  }

  val rootSchema: Decoder[RootSchema] = {
    for {
      id   <- property("$id")(string)
      defs <- optionalProperty("$defs")(map(schema))
      defs1 = defs.getOrElse(Map.empty[String, Schema])
      s <- schema
    } yield RootSchema(id, s, defs1)
  }

  val booleanSchema: Decoder[Schema] = value =>
    value match {
      case ObjectValue(_) =>
        Right(TrueSchema)
      case BoolValue(v) =>
        Right(
          if (v)
            (TrueSchema)
          else
            (FalseSchema)
        )
    }

  def typedSchema(`type`: String): Decoder[Schema] = `type` match {
    case "null"    => success(NullSchema)
    case "boolean" => success(BooleanSchema)
    case "string"  => success(StringSchema)
    case "number"  => success(NumberSchema)
    case "array"   => property("items")(schema).map(ArraySchema(_))
    case "object"  => property("properties")(map(schema)).map(ObjectSchema(_))
    case t @ _     => failure(s"unknown type $t")
  }

  val refSchema: Decoder[Option[Schema]] = optionalProperty("$ref")(string).map(s => s.map(RefSchema(_)))

  val schema: Decoder[Schema] = for {
    typedSchema <- ifObject(optionalProperty("type")(string)).flatMap(t => option(t.flatten.map(t => typedSchema(t))))
    refSchema   <- ifObject(refSchema).map(_.flatten)
    boolSchema  <- booleanSchema
    allOf       <- ifObject(optionalProperty("allOf")(seq(schema))).map(_.flatten).map(_.getOrElse(Seq()))
    anyOf       <- ifObject(optionalProperty("anyOf")(seq(schema))).map(_.flatten).map(_.getOrElse(Seq()))
    oneOf       <- ifObject(optionalProperty("oneOf")(seq(schema))).map(_.flatten).map(_.getOrElse(Seq()))
    notOp       <- ifObject(optionalProperty("not")(schema)).map(_.flatten)
    s = typedSchema.orElse(refSchema).getOrElse(boolSchema)
  } yield SchemaWithApplicators(s, allOf, anyOf, oneOf, notOp)

}

object Decoding {
  trait Decoder[+T] {
    def decode(value: Value): Either[String, T]
    def map[S](f: T => S): Decoder[S]              = flatMap(value => success(f(value)))
    def flatMap[S](f: T => Decoder[S]): Decoder[S] = andThen(this)(f)
  }

  def success[T](v: T): Decoder[T]      = value => Right(v)
  def failure[T](e: String): Decoder[T] = value => Left(e)

  def string: Decoder[String] = value =>
    value match {
      case StringValue(v) => Right(v)
      case v @ _          => Left(s"not a string '$v'")
    }

  def seq[T](d: Decoder[T]): Decoder[Seq[T]] = value =>
    value match {
      case ArrayValue(items) => Helper2.sequence(items.map(d.decode))
      case _                 => Left("not an array")
    }

  def option[T](d: Option[Decoder[T]]): Decoder[Option[T]] = value =>
    d match {
      case Some(d) => d.decode(value).map(Some(_))
      case _       => Right(None)
    }

  def map[K, V](d: Decoder[V]): Decoder[Map[String, V]] = value =>
    value match {
      case ObjectValue(properties) =>
        Helper2
          .sequence(properties.map { case (k, v) =>
            d.decode(v).map((k, _))
          }.toSeq)
          .map(Map.from(_))
      case _ => Left("not a map")
    }

  def ifObject[T](d: Decoder[T]): Decoder[Option[T]] = value =>
    value match {
      case ObjectValue(_) => d.decode(value).map(Some(_))
      case _              => Right(None)
    }

  def property[T](key: String)(d: Decoder[T]): Decoder[T] = value =>
    value match {
      case ObjectValue(properties) =>
        properties
          .get(key)
          .map(d.decode)
          .getOrElse(Left(s"missing property '$key'"))
      case _ => Left(s"not an object ${value} with ${key}")
    }

  def optionalProperty[T](key: String)(d: Decoder[T]): Decoder[Option[T]] = value =>
    value match {
      case ObjectValue(properties) =>
        properties
          .get(key)
          .map(d.decode)
          .map(_.map(Option(_)))
          .getOrElse(Right(None))
      case _ => Left(s"not an object ${value} with ${key}")
    }

  def andThen[S, T](d: Decoder[S])(f: S => Decoder[T]): Decoder[T] = value => {
    d
      .decode(value)
      .map(f)
      .flatMap(_.decode(value))
  }

  def map[S, T](d: Decoder[S])(f: S => T): Decoder[T] = d.map(f)

  def orElse[T](d: Decoder[T])(d2: Decoder[T]): Decoder[T] = value => {
    d
      .decode(value)
      .orElse(d2.decode(value))
  }

}

object Helper2 {
  // def sequence[T](options: Seq[Option[T]]): Option[Seq[T]] = {
  //   options.foldLeft(Option.empty[Seq[T]])((acc, v) =>
  //     v.flatMap(v =>
  //       acc
  //         .map(_ :+ v)
  //         .orElse(Option(Seq(v)))
  //     ).orElse(acc)
  //   )
  // }
  def sequence[E, T](eithers: Seq[Either[E, T]]): Either[E, Seq[T]] = {
    // TODO continue over Left?
    eithers.foldLeft[Either[E, Seq[T]]](Right[E, Seq[T]](Seq()))((acc, v) => acc.flatMap(acc => v.map(acc :+ _)))
  }

  def debugTraceValue[T](title: String): T => T = { v =>
    println(title, v)
    v
  }
}
