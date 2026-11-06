// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path
import io.circe.syntax.given
import lucuma.core.model.sequence.ghost.CentralWavelength
import lucuma.odb.json.wavelength.query.given

import table.GhostDynamicTable

trait GhostDynamicMapping[F[_]] extends GhostDynamicTable[F]:

  private def detectorMappingAtPath(path: Path, table: GhostDynamicTable[?], channel: table.Channel): ObjectMapping =
    ObjectMapping(path / channel.name)(
      SqlField("id", table.Id, key = true, hidden = true),
      SqlObject("exposureTime"),
      SqlField("exposureCount", channel.ExposureCount),
      SqlField("binning",       channel.Binning),
      SqlField("readMode",      channel.ReadMode)
    )

  private def dynamicMappingsAtPath(path: Path, table: GhostDynamicTable[?]): List[TypeMapping] =
    List(
      detectorMappingAtPath(path, table, table.Blue),
      detectorMappingAtPath(path, table, table.Red),
      ObjectMapping(path)(
        SqlField("id", table.Id, key = true, hidden = true),
        SqlObject("red"),
        SqlObject("blue"),
        SqlField("ifu1FiberAgitator", table.FiberAgitator.Ifu1),
        SqlField("ifu2FiberAgitator", table.FiberAgitator.Ifu2),
        CirceField("centralWavelength", CentralWavelength.asJson)
      )
    )

  // Generated steps are JSON without the fixed central wavelength.
  lazy val GhostDynamicMappings: List[TypeMapping] =
    ObjectMapping(GhostStepType / "instrumentConfig")(CirceField("centralWavelength", CentralWavelength.asJson)) ::
    dynamicMappingsAtPath(StepRecordType / "ghost", GhostDynamicTable) ++
    dynamicMappingsAtPath(CalibrationSetType / "ghost", GhostCalibrationSetupTable)
