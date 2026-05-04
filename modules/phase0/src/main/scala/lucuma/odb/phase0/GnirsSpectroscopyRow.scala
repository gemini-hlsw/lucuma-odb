// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.phase0

import cats.parse.Parser
import cats.syntax.all.*
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.Instrument
import lucuma.core.util.Enumerated

case class GnirsSpectroscopyRow(
  spec:    SpectroscopyRow,
  grating: GnirsGrating,
  filter:  GnirsFilter,
  fpu:     GnirsFpuSlit
)

object GnirsSpectroscopyRow:

  val gnirs: Parser[List[GnirsSpectroscopyRow]] =
    SpectroscopyRow.rows.flatMap: rs => // Only SingleSlit and XD for now, no IFU
      rs.filter(_.fpuOption === FpuOption.Singleslit).traverse: r =>
        val row = for
          _       <- Either.raiseWhen(r.instrument =!= Instrument.Gnirs)(s"Cannot parse a ${r.instrument.tag} as Gnirs")
          filter  <- r.filter
                       .toRight("GNIRS spectroscopy requires a filter")
                       .flatMap: f =>
                         Enumerated[GnirsFilter]
                           .all
                           .find(_.shortName === f)
                           .toRight(s"Cannot find filter: $f. Does a value exist in the Enumerated?")
          grating <- Enumerated[GnirsGrating]
                        .all
                        .find(_.shortName === r.disperser)
                        .toRight(s"Cannot find grating: ${r.disperser}. Does a value exist in the Enumerated?")
          fpu     <- Enumerated[GnirsFpuSlit]
                        .all
                        .find(_.slitWidth === r.slitWidth)
                        .toRight(s"Cannot find FPU: ${r.fpu}. Does a value exist in the Enumerated?")
        yield GnirsSpectroscopyRow(r, grating, filter, fpu)

        row.fold(Parser.failWith, Parser.pure)
