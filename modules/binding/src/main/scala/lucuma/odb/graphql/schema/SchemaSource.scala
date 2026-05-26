// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.schema

import cats.syntax.eq.*
import fs2.io.file.Path

import java.lang.System.lineSeparator
import scala.io.Source

trait SchemaSource:
  def resolve(name: Path): List[String]

object SchemaSource:
  def fromResource(classloader: ClassLoader): SchemaSource = (name: Path) =>
    Source.fromResource(name.toString, classloader).getLines().toList

  def fromString(name: Path, content: String): SchemaSource = (n: Path) =>
    if (n === name) content.split(lineSeparator).toList
    else throw new RuntimeException(s"Unknown source $n")

  def fromStringMap(m: Map[Path, String]): SchemaSource = (name: Path) =>
    m.get(name)
      .fold(throw new RuntimeException(s"Unknown source $name"))(_.split(lineSeparator).toList)
