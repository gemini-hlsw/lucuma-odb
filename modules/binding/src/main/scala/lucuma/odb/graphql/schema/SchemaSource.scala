// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.schema

import cats.syntax.eq.*
import fs2.io.file.Path

import java.lang.System.lineSeparator
import java.nio.file.Files
import scala.io.Source
import scala.util.Using
import scala.util.control.NonFatal

trait SchemaSource:
  def resolve(name: Path): List[String]

  def orElse(other: SchemaSource): SchemaSource = (name: Path) =>
    try resolve(name)
    catch case NonFatal(_) => other.resolve(name)

object SchemaSource:
  def fromResource(classloader: ClassLoader): SchemaSource = (name: Path) =>
    Using.resource(Source.fromResource(name.toString, classloader))(_.getLines().toList)

  def fromFileSystem(root: Path): SchemaSource = (name: Path) =>
    val file = root.toNioPath.resolve(name.toString).normalize
    if Files.exists(file) then Using.resource(Source.fromFile(file.toFile))(_.getLines().toList)
    else throw new RuntimeException(s"Unknown source $file")

  def fromString(name: Path, content: String): SchemaSource = (n: Path) =>
    if (n === name) content.split(lineSeparator).toList
    else throw new RuntimeException(s"Unknown source $n")

  def fromStringMap(m: Map[Path, String]): SchemaSource = (name: Path) =>
    m.get(name)
      .fold(throw new RuntimeException(s"Unknown source $name"))(_.split(lineSeparator).toList)
