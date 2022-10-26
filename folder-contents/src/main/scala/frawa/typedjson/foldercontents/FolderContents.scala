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
package frawa.typedjson.foldercontents

import java.nio.file.{Files, Path, Paths}

enum FolderContents[T]:
  case File(content: T)
  case Folder(items: Map[String, FolderContents[T]])

  def file(path: String): Option[T] =
    val p = Paths.get(path)
    files(p.getParent()).get(p.getFileName().toString)

  def files(path: String): Map[String, T] =
    files(Paths.get(path))

  def files(): Map[String, T] =
    files(null.asInstanceOf[Path])

  private def files(path: Path): Map[String, T] =
    if (path == null) then files(this)
    else folder(path).map(dir => files(dir)).getOrElse(Map())

  private def files(fc: FolderContents[T]): Map[String, T] =
    fc match {
      case File(_)           => Map()
      case folder: Folder[T] => files(folder)
    }

  private def folder(fc: FolderContents[T]): Option[Folder[T]] =
    fc match {
      case File(_)           => None
      case folder: Folder[T] => Some(folder)
    }

  private def folder(path: Path): Option[Folder[T]] =
    this match {
      case File(_) => None
      case Folder(items) =>
        if path.getNameCount == 1 then {
          items.get(path.getFileName().toString).flatMap(folder(_))
        } else {
          val head = path.getName(0).toString
          val tail = path.subpath(1, path.getNameCount - 1)
          items.get(head).flatMap(_.folder(tail))
        }
    }

  private def files(folder: Folder[T]): Map[String, T] =
    folder.items.flatMap {
      case (name, File(content)) => Some((name, content))
      case _                     => None
    }.toMap
