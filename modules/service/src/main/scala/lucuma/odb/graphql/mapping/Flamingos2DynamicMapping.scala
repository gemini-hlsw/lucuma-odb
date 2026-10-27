// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path

import table.Flamingos2DynamicTables

trait Flamingos2DynamicMapping[F[_]] extends Flamingos2DynamicTables[F]:

  private def dynamicMappingAtPath(
    path:  Path,
    table: Flamingos2DynamicTable[?]
  ): ObjectMapping =
    ObjectMapping(path)(
      SqlField("id",          table.Id, key = true, hidden = true),
      SqlObject("exposure"),
      SqlField("disperser",   table.Disperser),
      SqlField("filter",      table.Filter),
      SqlField("readMode",    table.ReadMode),
      SqlField("lyotWheel",   table.LyotWheel),
      SqlObject("fpu"),
      SqlField("decker",      table.Decker),
      SqlField("readoutMode", table.ReadoutMode),
      SqlField("reads",       table.Reads),
      SqlField("windowCover", table.WindowCover),
      SqlObject("centralWavelength")
    )

  lazy val Flamingos2DynamicMappings: List[TypeMapping] =
    List(
      dynamicMappingAtPath(StepRecordType / "flamingos2", Flamingos2DynamicView),
      dynamicMappingAtPath(CalibrationSetType / "flamingos2", Flamingos2CalibrationSetupTable)
    )