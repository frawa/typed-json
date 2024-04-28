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

package frawa.typedjson.schematestsuite.data

import frawa.inlinefiles.InlineFiles
import frawa.inlinefiles.InlineFiles._

object Draft202012:

  private val all = inlineDeepTextFiles("./JSON-Schema-Test-Suite/tests/draft2020-12", ".json")

  val files               = all.files()
  val optionalFiles       = all.folder("optional").files()
  val optionalFormatFiles = all.folder("optional/format").files()
  val remotesFiles        = inlineDeepTextFiles("./JSON-Schema-Test-Suite/remotes", ".json")
