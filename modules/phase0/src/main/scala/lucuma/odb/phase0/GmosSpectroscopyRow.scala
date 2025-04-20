// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
    u:    U => String
  ): Parser[List[GmosSpectroscopyRow[G, L, U]]] =
    // NOTE: If any of the Enumerated instances for Grating (Disperser), Filter or Fpu are changed, the
    // file probably needs to be reimported (See `R__Phase0.importForcingVersion`), or the 
    // import file needs to modified. In particular, if a Grating or Filter is obsoleted and replaced by
    // a new one with the same `shortName`, the file needs to be re-imported (via importForcingVersion). 
    // If one is obsoleted and not replaced, the relevant rows need to be removed from the import file (which
    // will automatically trigger a re-import).
    SpectroscopyRow.rows.flatMap { rs =>
      rs.traverse { r =>
        val gn = for {
          _ <- Either.raiseWhen(r.instrument =!= inst)(s"Cannot parse a ${r.instrument.tag} as ${inst.tag}")
          g <- Enumerated[G].all.find(a => g(a) === r.disperser).toRight(s"Cannot find disperser: ${r.disperser}. Does a value exist in the Enumerated?")
          l <- r.filter.traverse { f => Enumerated[L].all.find(a => l(a) === f).toRight(s"Cannot find filter: $f. Does a value exist in the Enumerated?") }
          u <- Enumerated[U].all.find(a => u(a) === r.fpu).toRight(s"Cannot find FPU: ${r.fpu}. Does a value exist in the Enumerated?")
        } yield GmosSpectroscopyRow(r, g, l, u)
        gn.fold(Parser.failWith, Parser.pure)
      }
    }

  val gmosNorth: Parser[List[GmosNorth]] =
    gmos[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu](Instrument.GmosNorth, _.shortName, _.shortName, _.shortName)

  val gmosSouth: Parser[List[GmosSouth]] =
    gmos[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu](Instrument.GmosSouth, _.shortName, _.shortName, _.shortName)

}
