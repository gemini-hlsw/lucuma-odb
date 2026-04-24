// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.schema

import cats.effect.Sync
import cats.syntax.eq.*
import fs2.RaiseThrowable
import fs2.Stream
import fs2.io
import fs2.io.file.Path
import fs2.text

trait SchemaSource[F[_]]:
  def resolve(name: Path): Stream[F, String]

object SchemaSource:
  def fromResource[F[_]: Sync]: SchemaSource[F] = (name: Path) =>
    Stream
      .eval(Sync[F].blocking(Option(getClass.getClassLoader.getResourceAsStream(name.toString))))
      .flatMap:
        case Some(resource) =>
          io.readInputStream(Sync[F].pure(resource), 8192)
            .through(text.utf8.decode)
            .through(text.lines)
        case None           => Stream.raiseError[F](new io.IOException(s"Resource $name not found"))

  def fromString[F[_]: RaiseThrowable](name: Path, content: String): SchemaSource[F] = (n: Path) =>
    if (n === name) Stream.emit(content).through(text.lines)
    else Stream.raiseError[F](new Error(s"Unknown source $n"))

  def fromStringMap[F[_]: RaiseThrowable](m: Map[Path, String]): SchemaSource[F] = (name: Path) =>
    m.get(name)
      .fold(Stream.raiseError[F](new Error(s"Unknown source $name")))(x =>
        Stream.emit(x).through(text.lines)
      )
