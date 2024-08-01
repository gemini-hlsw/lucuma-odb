// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.phase0

import cats.ApplicativeError
import cats.parse.Parser
import cats.syntax.eq.*
import cats.syntax.functor.*
import cats.syntax.show.*
import eu.timepit.refined.types.numeric.PosInt
import fs2.Pipe
import fs2.Stream
import fs2.text
import lucuma.core.enums.Instrument

class FileReader[F[_]](fileName: String)(using ApplicativeError[F, Throwable]) {

  final class ReadException(
    val lineNumber: PosInt,
    val error:      Parser.Error
  ) extends RuntimeException(s"$fileName, line ${lineNumber.value}:\n${error.show}")

  def tooManyLines[A]: Stream[F, A] =
    Stream.raiseError(new RuntimeException(s"$fileName contains too many lines."))

  def zipWithLineNumber[A]: Pipe[F, A, (A, PosInt)] =
    _.zipWithIndex
     .map(_.map(_ + 1))
     .flatMap { case (a, n) =>
       Option
         .when(n.isValidInt)(PosInt.unsafeFrom(n.toInt))
         .fold(tooManyLines[(A, PosInt)]) { idx => Stream.emit((a, idx))}
     }

  val entryLines: Pipe[F, Byte, (String, PosInt)] =
    _.through(text.utf8.decode)
     .through(text.lines)
     .map(_.trim)
     .through(zipWithLineNumber)

  def read[A](i: Instrument, p: Parser[List[A]]): Pipe[F, Byte, (A, PosInt)] =
    _.through(entryLines)
     .filter { case (s, _) => s.startsWith(i.shortName) }
     .flatMap { case (s, idx) =>
       p.parseAll(s) match {
         case Left(e)   => Stream.raiseError[F](new ReadException(idx, e))
         case Right(as) => Stream.emits[F, A](as)
       }
     }
     .through(zipWithLineNumber)

  val gmosNorth: Pipe[F, Byte, (GmosSpectroscopyRow.GmosNorth, PosInt)] =
    read(Instrument.GmosNorth, GmosSpectroscopyRow.gmosNorth)
      .andThen(_.filter(_._1.spec.fpuOption === FpuOption.Singleslit)) // for now only single slit
      .andThen(_.filter(_._1.spec.capability.isEmpty))                 // for now no N&S

  def gmosSouth: Pipe[F, Byte, (GmosSpectroscopyRow.GmosSouth, PosInt)] =
    read(Instrument.GmosSouth, GmosSpectroscopyRow.gmosSouth)
      .andThen(_.filter(_._1.spec.fpuOption === FpuOption.Singleslit)) // for now only single slit
      .andThen(_.filter(_._1.spec.capability.isEmpty))                 // for now no N&S

}
