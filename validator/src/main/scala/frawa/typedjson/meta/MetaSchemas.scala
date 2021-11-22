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

package frawa.typedjson.meta

import frawa.typedjson.macros.Macros
import frawa.typedjson.util.UriUtil.uri
import frawa.typedjson.schema.{LoadedSchemasResolver, SchemaValue}
import frawa.typedjson.parser._

import java.net.URI

object MetaSchemas {

  val draft202012: URI = uri("https://json-schema.org/draft/2020-12/")

  def lazyResolver: LoadedSchemasResolver.LazyResolver = { uri =>
    if (uri.getSchemeSpecificPart.startsWith(draft202012.getSchemeSpecificPart)) {
      resolve202012(draft202012.relativize(uri))
    } else { None }
  }

  private def resolve202012(relative: URI): Option[SchemaValue] = {
    val name = relative.getSchemeSpecificPart
    name match {
      case "schema"                 => Some(schemaContent)
      case "meta/applicator"        => Some(applicatorContent)
      case "meta/content"           => Some(contentContent)
      case "meta/core"              => Some(coreContent)
      case "meta/format-annotation" => Some(formatAnnotationContent)
      case "meta/meta-data"         => Some(metaDataContent)
      case "meta/unevaluated"       => Some(unevaluatedContent)
      case "meta/validation"        => Some(validationContent)
      case _                        => None
    }
  }

  import Macros._

  private val schemaContent           = SchemaValue(jsonContent("./metaSchemas/schema.json"))
  private val applicatorContent       = SchemaValue(jsonContent("./metaSchemas/meta/applicator.json"))
  private val contentContent          = SchemaValue(jsonContent("./metaSchemas/meta/content.json"))
  private val coreContent             = SchemaValue(jsonContent("./metaSchemas/meta/core.json"))
  private val formatAnnotationContent = SchemaValue(jsonContent("./metaSchemas/meta/format-annotation.json"))
  private val metaDataContent         = SchemaValue(jsonContent("./metaSchemas/meta/meta-data.json"))
  private val unevaluatedContent      = SchemaValue(jsonContent("./metaSchemas/meta/unevaluated.json"))
  private val validationContent       = SchemaValue(jsonContent("./metaSchemas/meta/validation.json"))
}
