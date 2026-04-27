// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal.parsers

import cats.parse.Parser
import cats.parse.Parser0
import cats.syntax.all.*
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.parser.MiscParsers.posInt
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.core.util.parser.UtilParsers.posSecondsTimeSpan
import lucuma.odb.smartgcal.data.Ghost.GhostUpdate
import lucuma.odb.smartgcal.data.Ghost.SearchKey
import lucuma.odb.smartgcal.data.Ghost.TableRow
import lucuma.odb.smartgcal.data.SmartGcalValue

trait GhostParsers:
  import common.*
  import gcal.*
  import util.*

  val binning: Parser[GhostBinning] =
    oneOf(Enumerated[GhostBinning].all.fproductLeft(_.name)*).withContext("GHOST binning")

  val resolution: Parser[GhostResolutionMode] =
    oneOf(Enumerated[GhostResolutionMode].all.fproductLeft(_.longName)*).withContext("GHOST resolution mode")

  val key: Parser[SearchKey] =
    (
      (resolution <* columnSep) ~
      (skipColumn *> binning)
    ).map { case (r, b) => SearchKey(r, b, b) }  // for now, we assume the binning applies to both red and blue

  private val seconds: Parser0[TimeSpan] =
    posSecondsTimeSpan <* Parser.char('s')

  val value: Parser[SmartGcalValue[GhostUpdate]] =
    (
      (posInt       <* columnSep) ~  // observe count (step count)
      (stepConfig   <* columnSep) ~
      (seconds      <* columnSep) ~  // red exposure time
      (posInt       <* columnSep) ~  // red exposure count
      (seconds      <* columnSep) ~  // blue exposure time
      (posInt       <* columnSep) ~  // blue exposure count
      (baselineType <* columnSep) <* (skipColumns(6) <* ignoredValue)
    ).map { case (((((((cnt, gcal), redTime), redCount)), blueTime), blueCount), baseline) =>
      SmartGcalValue(gcal, baseline, cnt, GhostUpdate(redTime, redCount, blueTime, blueCount))
    }

  val row: Parser[TableRow] =
    ((key <* columnSep) ~ value).map((key, value) => TableRow(key, value))

object ghost extends GhostParsers