// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal.parsers

import cats.parse.Parser
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.core.parser.MiscParsers.posInt
import lucuma.core.util.parser.UtilParsers.posSecondsTimeSpan
import lucuma.odb.smartgcal.data.Igrins2.FileEntry
import lucuma.odb.smartgcal.data.Igrins2.FileKey
import lucuma.odb.smartgcal.data.SmartGcalValue
import lucuma.odb.smartgcal.data.SmartGcalValue.LegacyInstrumentConfig

trait Igrins2Parsers:
  import common.*

  private val mode: Parser[Unit] =
    Parser.string("Spectroscopy").void

  val fileEntry: Parser[FileEntry] =
    (
      (mode                <* columnSep) ~
      (gcal.gcalLamp       <* columnSep) ~
      (gcal.shutter        <* columnSep) ~
      (gcal.filter         <* columnSep) ~
      (gcal.diffuser       <* columnSep) ~
      (posInt              <* columnSep) ~
      (posSecondsTimeSpan  <* columnSep) ~
      (posInt              <* columnSep) ~
      gcal.baselineType
    ).map { case ((((((((_, lamp), shutter), filter), diffuser), count), time), coadds), baseline) =>
      FileEntry(
        FileKey(),
        SmartGcalValue(
          Gcal(lamp, filter, diffuser, shutter),
          baseline,
          count,
          LegacyInstrumentConfig(time, coadds)
        )
      )
    }

object igrins2 extends Igrins2Parsers
