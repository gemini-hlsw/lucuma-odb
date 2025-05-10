// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.phase0

import cats.parse.*
import cats.syntax.all.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.Instrument
import lucuma.core.util.Enumerated

case class GmosImagingRow[L](
  img:    ImagingRow,
  filter: L
)

object GmosImagingRow {

  type GmosNorth = GmosImagingRow[GmosNorthFilter]
  type GmosSouth = GmosImagingRow[GmosSouthFilter]

  def gmos[L: Enumerated](
    inst: Instrument,
    p:    L => String,
  ): Parser[List[GmosImagingRow[L]]] =
    ImagingRow.rows.flatMap { rs =>
      rs.traverse { r =>
        val gn = for {
          _ <- Either.raiseWhen(r.instrument =!= inst)(s"Cannot parse a ${r.instrument.tag} as ${inst.tag}")
          l <- Enumerated[L].all.find(a => p(a) === r.filter).toRight(s"Cannot find filter: ${r.filter}. Does a value exist in the Enumerated?")
        } yield GmosImagingRow(r, l)
        gn.fold(Parser.failWith, Parser.pure)
      }
    }

  val gmosNorth: Parser[List[GmosNorth]] =
    gmos[GmosNorthFilter](Instrument.GmosNorth, _.shortName)

  val gmosSouth: Parser[List[GmosSouth]] =
    gmos[GmosSouthFilter](Instrument.GmosSouth, _.shortName)

}
