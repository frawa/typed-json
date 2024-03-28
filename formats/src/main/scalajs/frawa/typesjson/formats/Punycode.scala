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

package frawa.typesjson.formats

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

// see https://github.com/garycourt/uri-js

// https://www.scala-js.org/doc/interoperability/sjs-defined-js-classes.html
// TOOD useful later

// trait URIComponents extends js.Object {
//   val scheme: js.UndefOr[String]     = js.undefined
//   val userinfo: js.UndefOr[String]   = js.undefined
//   val host: js.UndefOr[String]       = js.undefined
//   val port: js.UndefOr[String | Int] = js.undefined
//   val path: js.UndefOr[String]       = js.undefined
//   val query: js.UndefOr[String]      = js.undefined
//   val fragment: js.UndefOr[String]   = js.undefined
//   val reference: js.UndefOr[String]  = js.undefined
//   val error: js.UndefOr[String]      = js.undefined
// }
// trait URIOptions extends js.Object {
//   val scheme: js.UndefOr[String]          = js.undefined
//   val reference: js.UndefOr[String]       = js.undefined
//   val tolerant: js.UndefOr[Boolean]       = js.undefined
//   val absolutePath: js.UndefOr[Boolean]   = js.undefined
//   val iri: js.UndefOr[Boolean]            = js.undefined
//   val unicodeSupport: js.UndefOr[Boolean] = js.undefined
//   val domainHost: js.UndefOr[Boolean]     = js.undefined
// }
// object UriJsArguments {
//   def componentsHost(host: String): URIComponents =
//     js.Dynamic.literal("host" -> host).asInstanceOf[URIComponents]
//   def options(): URIOptions =
//     new URIOptions {
//       override val unicodeSupport = true
//       override val domainHost     = true
//     }
// }

@js.native
@JSImport("punycode", JSImport.Namespace)
object Punycode extends js.Object {

  def toASCII(input: String): String =
    js.native

  def toUnicode(input: String): String =
    js.native

}
