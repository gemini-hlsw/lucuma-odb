// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal

import cats.ApplicativeError
import cats.parse.Parser
import cats.syntax.show.*
import eu.timepit.refined.types.numeric.PosLong
import fs2.Pipe
import fs2.Pull
import fs2.RaiseThrowable
import fs2.Stream
import fs2.text
import lucuma.odb.smartgcal.parsers.Availability
import lucuma.odb.smartgcal.parsers.Availability.*

object FileReader {

  final class ReadException(
    val fileName:   String,
    val lineNumber: PosLong,
    val error:      Parser.Error
  ) extends RuntimeException(s"$fileName, line $lineNumber: ${error.show}")

  final class HeaderException(
    val fileName: String,
    val reason:   String
  ) extends RuntimeException(s"$fileName: $reason")

  private val VersionLine = """\d+,\d{4}/\d{2}/\d{2}.*""".r

  // Every file's column header names this column and no data row does.
  private def isHeader(s: String): Boolean =
    s.contains("Calibration Observe")

  /**
   * The data lines of a file with their line numbers.  Comments and blank
   * lines are skipped, then an optional version line and the column header,
   * which must be present.  The header is recognized rather than counted, so a
   * file that gains or loses its version line keeps every data row.
   */
  def entryLines[F[_]: RaiseThrowable](fileName: String): Pipe[F, Byte, (PosLong, String)] = { in =>
    def skip(s: String): Boolean =
      s.startsWith("#") || s.startsWith(",") || s.isEmpty

    def fail(reason: String): Pull[F, Nothing, Unit] =
      Pull.raiseError[F](new HeaderException(fileName, reason))

    def header(lines: Stream[F, (PosLong, String)]): Pull[F, (PosLong, String), Unit] =
      lines.pull.uncons1.flatMap:
        case Some(((_, s), rest)) if isHeader(s) => rest.pull.echo
        case Some(((n, s), _))                   => fail(s"line $n: expected the column header, found: $s")
        case None                                => fail("no column header found")

    val lines =
      in.through(text.utf8.decode)
        .through(text.lines)
        .map(_.trim)
        .zipWithIndex
        .collect:
          case (s, n) if !skip(s) => (PosLong.unsafeFrom(n + 1), s)

    lines.pull.uncons1.flatMap {
      case Some(((_, s), rest)) if VersionLine.matches(s) => header(rest)
      case Some((first, rest))                            => header(rest.cons1(first))
      case None                                           => fail("no column header found")
    }.stream
  }

  def read[F[_], A](
    n: String,
    p: Parser[Availability[A]]
  )(using ApplicativeError[F, Throwable]): Pipe[F, Byte, (PosLong, A)] =
    _.through(entryLines(n))
     .flatMap: (lineNumber, s) =>
       p.parseAll(s) match
         case Left(e)           => Stream.raiseError[F](new ReadException(n, lineNumber, e))
         case Right(Obsolete)   => Stream.empty
         case Right(Current(a)) => Stream.emit[F, (PosLong, A)](lineNumber -> a)

  def ghost[F[_]](fileName: String)(using ApplicativeError[F, Throwable]): Pipe[F, Byte, (PosLong, data.Ghost.TableRow)] =
    read(fileName, parsers.ghost.row.map(Current.apply))

  def gmosNorth[F[_]](fileName: String)(using ApplicativeError[F, Throwable]): Pipe[F, Byte, (PosLong, data.Gmos.FileEntry.North)] =
    read(fileName, parsers.gmosNorth.fileEntry)

  def gmosSouth[F[_]](fileName: String)(using ApplicativeError[F, Throwable]): Pipe[F, Byte, (PosLong, data.Gmos.FileEntry.South)] =
    read(fileName, parsers.gmosSouth.fileEntry)

  def flamingos2[F[_]](fileName: String)(using ApplicativeError[F, Throwable]): Pipe[F, Byte, (PosLong, data.Flamingos2.FileEntry)] =
    read(fileName, parsers.flamingos2.fileEntry.map(Current.apply))

  def igrins2[F[_]](fileName: String)(using ApplicativeError[F, Throwable]): Pipe[F, Byte, (PosLong, data.Igrins2.FileEntry)] =
    read(fileName, parsers.igrins2.fileEntry.map(Current.apply))

  def gnirs[F[_]](fileName: String)(using ApplicativeError[F, Throwable]): Pipe[F, Byte, (PosLong, data.Gnirs.FileEntry)] =
    read(fileName, parsers.gnirs.fileEntry.map(Current.apply))
}
