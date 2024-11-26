// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.phase0

import cats.parse.*
import cats.syntax.all.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.Instrument
import lucuma.core.util.Enumerated

case class GmosSpectroscopyRow[G, L, U](
  spec:      SpectroscopyRow,
  disperser: G,
  filter:    Option[L],
  fpu:       U
)

object GmosSpectroscopyRow {

  type GmosNorth = GmosSpectroscopyRow[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]
  type GmosSouth = GmosSpectroscopyRow[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]

  def gmos[G: Enumerated, L: Enumerated, U: Enumerated](
    inst: Instrument,
    g:    G => String,
    l:    L => String,
    u:    U => String,
    gObs: G => Boolean,
    lObs: L => Boolean

  ): Parser[List[GmosSpectroscopyRow[G, L, U]]] =
    SpectroscopyRow.rows.flatMap { rs =>
      rs.traverse { r =>
        val gn = for {
          _ <- Either.raiseWhen(r.instrument =!= inst)(s"Cannot parse a ${r.instrument.tag} as ${inst.tag}")
          g <- Enumerated[G].all.find(a => g(a) === r.disperser && !gObs(a)).toRight(s"Cannot parse disperser: ${r.disperser}")
          l <- r.filter.traverse { f => Enumerated[L].all.find(a => l(a) === f && !lObs(a)).toRight(s"Cannot parse filter: $f") }
          u <- Enumerated[U].all.find(a => u(a) === r.fpu).toRight(s"Cannot parse FPU: ${r.fpu}")
        } yield GmosSpectroscopyRow(r, g, l, u)
        gn.fold(Parser.failWith, Parser.pure)
      }
    }

  val gmosNorth: Parser[List[GmosNorth]] =
    gmos[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu](Instrument.GmosNorth, _.shortName, _.shortName, _.shortName, _.obsolete, _.obsolete)

  val gmosSouth: Parser[List[GmosSouth]] =
    gmos[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu](Instrument.GmosSouth, _.shortName, _.shortName, _.shortName, _.obsolete, _.obsolete)

}
