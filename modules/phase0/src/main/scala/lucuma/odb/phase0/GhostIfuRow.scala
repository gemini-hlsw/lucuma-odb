// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.phase0

import cats.parse.*
import cats.syntax.all.*
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.enums.Instrument
import lucuma.core.util.Enumerated

case class GhostIfuRow(
  spec:           SpectroscopyRow,
  resolutionMode: GhostResolutionMode,
  binning:        GhostBinning
)

object GhostIfuRow:

  private val ResolutionMapping: Map[String, GhostResolutionMode] =
    Map(
      "SR-IFU" -> GhostResolutionMode.Standard,
      "HR-IFU" -> GhostResolutionMode.High
    )

  private def resolutionMode(r: SpectroscopyRow): Either[String, GhostResolutionMode] =
    ResolutionMapping
      .get(r.fpu)
      .toRight(s"Cannot find GHOST resolution mode ${r.fpu}")

  private def binning(r: SpectroscopyRow): Either[String, GhostBinning] =
    r.description
     .split(' ')
     .drop(1)
     .headOption
     .flatMap: binString =>
       Enumerated[GhostBinning].all.find(binString === _.name)
     .toRight(s"GHOST binning not recognized in row description: ${r.description}")

  def ghost: Parser[List[GhostIfuRow]] =
    SpectroscopyRow.rows.flatMap: rs =>
      rs.traverse: r =>
        val row =
          for
            _ <- Either.raiseWhen(r.instrument =!= Instrument.Ghost)(s"Cannot parse a ${r.instrument.tag} as GHOST")
            m <- resolutionMode(r)
            b <- binning(r)
          yield GhostIfuRow(r, m, b)
        row.fold(Parser.failWith, Parser.pure)