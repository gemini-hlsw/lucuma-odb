// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.phase0

import cats.parse.*
import cats.syntax.all.*
import lucuma.core.enums.F2Disperser
import lucuma.core.enums.F2Filter
import lucuma.core.enums.F2Fpu
import lucuma.core.enums.Instrument
import lucuma.core.util.Enumerated

case class F2SpectroscopyRow(
  spec:      SpectroscopyRow,
  disperser: F2Disperser,
  filter:    F2Filter,
  fpu:       F2Fpu
)

object F2SpectroscopyRow:

  val f2: Parser[List[F2SpectroscopyRow]] =
    SpectroscopyRow.rows.flatMap: rs =>
      rs.traverse: r =>
        val row = for {
          _ <- Either.raiseWhen(r.instrument =!= Instrument.Flamingos2)(s"Cannot parse a ${r.instrument.tag} as Flamingos2")
          g <- Enumerated[F2Disperser].all.find(_.shortName === r.disperser).toRight(s"Cannot find disperser: ${r.disperser}. Does a non-obsolete value exist in the Enumerated?")
          l <- r.filter
                 .toRight("Flamingos 2 spectroscopy requires a filter")
                 .flatMap: f =>
                    Enumerated[F2Filter].all.find(a => a.shortName === f)
                      .toRight(s"Cannot find filter: $f. Does a non-obsolete value exist in the Enumerated?")
          u <- Enumerated[F2Fpu].all.find(_.shortName === r.fpu).toRight(s"Cannot find FPU: ${r.fpu}. Does a value exist in the Enumerated?")
        } yield F2SpectroscopyRow(r, g, l, u)
        row.fold(Parser.failWith, Parser.pure)

