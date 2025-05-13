// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.phase0

import cats.parse.*
import cats.parse.Rfc5234.sp
import cats.parse.Rfc5234.vchar
import cats.syntax.all.*
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.Instrument
import lucuma.core.parser.MiscParsers.int
import lucuma.core.util.Enumerated

case class Flamingos2SpectroscopyRow(
  spec:      SpectroscopyRow,
  disperser: Flamingos2Disperser,
  filter:    Flamingos2Filter,
  fpu:       Flamingos2Fpu
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
          // In the table they are written as "n\s?pixels"
          u <- Enumerated[Flamingos2Fpu]
                 .all
                 .find(f => fpuParser.parse(r.fpu).map(_._2).forall(_ === f.slitWidth.value))
                 .toRight(s"Cannot find FPU: ${r.fpu}. Does a value exist in the Enumerated?")
        } yield Flamingos2SpectroscopyRow(r, g, l, u)
        row.fold(Parser.failWith, Parser.pure)

