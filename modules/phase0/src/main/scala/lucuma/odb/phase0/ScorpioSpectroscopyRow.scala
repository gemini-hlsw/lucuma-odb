// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.phase0

import cats.parse.Parser
import cats.syntax.all.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.ScorpioFpu
import lucuma.core.util.Enumerated

case class ScorpioSpectroscopyRow(
  spec: SpectroscopyRow,
  fpu:  ScorpioFpu
)

object ScorpioSpectroscopyRow:

  // SCORPIO exposes all channels at once, so the slit alone identifies an option.
  val scorpio: Parser[List[ScorpioSpectroscopyRow]] =
    SpectroscopyRow.rows.flatMap: rs =>
      rs.filter(_.fpuOption === FpuOption.Singleslit).traverse: r =>
        val row = for
          _   <- Either.raiseWhen(r.instrument =!= Instrument.Scorpio)(s"Cannot parse a ${r.instrument.tag} as Scorpio")
          fpu <- Enumerated[ScorpioFpu]
                   .all
                   .find(_.slitWidth === r.slitWidth)
                   .toRight(s"Cannot find FPU with slit width: ${r.fpu}. Does a value exist in the Enumerated?")
        yield ScorpioSpectroscopyRow(r, fpu)

        row.fold(Parser.failWith, Parser.pure)
