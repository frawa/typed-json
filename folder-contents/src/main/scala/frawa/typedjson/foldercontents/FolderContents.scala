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

enum FolderContents[T]:
  case File(content: T)
  case Folder(items: Map[String, FolderContents[T]])

  private type Path = Seq[String]

  private def toPath(path: String): Path      = path.split("/").toSeq
  private def getParent(path: Path): Path     = path.slice(0, path.length - 1)
  private def getFileName(path: Path): String = path.last

  def file(path: String): Option[T] =
    val p      = toPath(path)
    val parent = getParent(p)
    val file   = getFileName(p)
    files(parent).get(p.last)

  def files(path: String): Map[String, T] =
    files(toPath(path))

  def files(): Map[String, T] =
    files(Seq())

  private def files(path: Path): Map[String, T] =
    if (path.isEmpty) then files(this)
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
        if path.size == 1 then {
          items.get(getFileName(path)).flatMap(folder(_))
        } else {
          items.get(path.head).flatMap(_.folder(path.tail))
        }
    }

  private def files(folder: Folder[T]): Map[String, T] =
    folder.items.flatMap {
      case (name, File(content)) => Some((name, content))
      case _                     => None
    }.toMap
