// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal

import cats.ApplicativeError
import cats.parse.Parser
import cats.syntax.functor.*
import cats.syntax.show.*
import fs2.Pipe
import fs2.Stream
import fs2.text

object FileReader {

  final class ReadException(
    val fileName:   String,
    val lineNumber: Long,
    val error:      Parser.Error
  ) extends RuntimeException(s"$fileName, line $lineNumber: ${error.show}")

  def entryLines[F[_]]: Pipe[F, Byte, (Long, String)] = {
    def skip(s: String): Boolean =
      s.startsWith("#") || s.startsWith(",") || s.isEmpty

    _.through(text.utf8.decode)
     .through(text.lines)
     .map(_.trim)
     .zipWithIndex
     .collect {
       case (s, n) if !skip(s) => (n+1, s)  // convert index to line number, skip comments, swap order
     }
     .drop(2)                               // drop version and header
  }

  def read[F[_], A](n: String, p: Parser[A])(using ApplicativeError[F, Throwable]): Pipe[F, Byte, A] =
    _.through(entryLines)
     .flatMap { case (lineNumber, s) =>
       p.parseAll(s) match {
         case Left(e)  => Stream.raiseError[F](new ReadException(n, lineNumber, e))
         case Right(a) => Stream.emit[F, A](a)
       }
     }

  def gmosNorth[F[_]](fileName: String)(using ApplicativeError[F, Throwable]): Pipe[F, Byte, data.GmosNorth.FileEntry] =
    read(fileName, parsers.gmosNorth.fileEntry)

}
