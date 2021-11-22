package frawa.typedjson.schema

import frawa.typedjson.parser.Value

case class InnerValue(value: Value, pointer: Pointer = Pointer.empty)
