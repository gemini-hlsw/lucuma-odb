// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal.parsers

import cats.parse.Parser
import lucuma.core.parser.MiscParsers.comma
import lucuma.core.parser.MiscParsers.posInt
import lucuma.core.util.parser.UtilParsers.posSecondsTimeSpan
import lucuma.odb.smartgcal.data.FileVersion
import lucuma.odb.smartgcal.data.SmartGcalValue
import lucuma.odb.smartgcal.data.SmartGcalValue.Legacy
import lucuma.odb.smartgcal.data.SmartGcalValue.LegacyInstrumentConfig


trait SmartGcalFileParsers {
  import common.*

  val version: Parser[FileVersion] =
    ((posInt <* Parser.char(',')) ~ instant).map { case (n, i) =>
      FileVersion(n, i)
    }

  val keyValueSep: Parser[Unit] =
    comma <* comma <* comma <* comma

  /**
   * Legacy SmartGal files only specify exposure time and "coadds" whether they
   * make sense for the instrument or not.
   */
  val legacyValue: Parser[Legacy] =
    (
      (posInt             <* columnSep) ~  // count
      (gcal.stepConfig    <* columnSep) ~
      (posSecondsTimeSpan <* columnSep) ~ // exposure time
      (posInt             <* columnSep) ~ // coadds
      gcal.baselineType
    ).map { case ((((count, stepConfig), time), coadds), baseline) =>
      SmartGcalValue(
        stepConfig,
        baseline,
        count,
        LegacyInstrumentConfig(
          time,
          coadds
        )
      )
    }

}

object file extends SmartGcalFileParsers

