// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.phase0

import cats.parse.*
import cats.parse.Rfc5234.sp
import cats.parse.Rfc5234.vchar
import cats.syntax.all.*
import lucuma.core.enums.Flamingos2CustomSlitWidth
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.Instrument
import lucuma.core.parser.MiscParsers.int
import lucuma.core.util.Enumerated

case class Flamingos2SpectroscopyRow(
  spec:            SpectroscopyRow,
  disperser:       Flamingos2Disperser,
  filter:          Flamingos2Filter,
  fpu:             Option[Flamingos2Fpu],
  customSlitWidth: Option[Flamingos2CustomSlitWidth]
)

object Flamingos2SpectroscopyRow:

  val fpuParser: Parser[Int] =
    (int ~ sp.? ~ vchar.rep.?).mapFilter { case ((i, _), _) =>
      i.some
    }

  val flamingos2: Parser[List[Flamingos2SpectroscopyRow]] =
    SpectroscopyRow.rows.flatMap: rs =>
      rs.traverse: r =>
        val row = for {
          _ <- Either.raiseWhen(r.instrument =!= Instrument.Flamingos2)(s"Cannot parse a ${r.instrument.tag} as Flamingos2")
          g <- Enumerated[Flamingos2Disperser].all.find(_.shortName === r.disperser).toRight(s"Cannot find disperser: ${r.disperser}. Does a non-obsolete value exist in the Enumerated?")
          l <- r.filter
                 .toRight("Flamingos 2 spectroscopy requires a filter")
                 .flatMap: f =>
                    Enumerated[Flamingos2Filter].all.find(a => a.shortName === f)
                      .toRight(s"Cannot find filter: $f. Does a non-obsolete value exist in the Enumerated?")
          p <- fpuParser.parse(r.fpu)
                 .bimap(_ => s"Cannot parse FPU pixel width: ${r.fpu}", _._2)
          // For MOS rows the focal plane is a custom mask, not a builtin slit, so we return none.
          u <- (if (r.fpuOption === FpuOption.Multislit) none[Flamingos2Fpu].asRight[String]
                else Enumerated[Flamingos2Fpu]
                       .all
                       .find(_.slitWidth.value === p)
                       .map(_.some)
                       .toRight(s"Cannot find FPU: ${r.fpu}. Does a value exist in the Enumerated?"))
          w <- (if (r.fpuOption === FpuOption.Multislit)
                  Enumerated[Flamingos2CustomSlitWidth]
                    .all
                    .find(_.fpu.exists(_.slitWidth.value === p))
                    .map(_.some)
                    .toRight(s"Cannot find custom slit width: ${r.fpu}. Does a value exist in the Enumerated?")
                else none[Flamingos2CustomSlitWidth].asRight[String])
        } yield Flamingos2SpectroscopyRow(r, g, l, u, w)
        row.fold(Parser.failWith, Parser.pure)
