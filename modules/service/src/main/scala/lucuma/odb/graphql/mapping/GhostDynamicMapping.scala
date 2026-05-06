// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import io.circe.syntax.given
import lucuma.core.model.sequence.ghost.CentralWavelength
import lucuma.odb.json.wavelength.query.given

import table.GhostDynamicTable

trait GhostDynamicMapping[F[_]] extends GhostDynamicTable[F]:

  def detectorMapping(channel: GhostDynamicTable.Channel): ObjectMapping =
    ObjectMapping(GhostDynamicType / channel.name)(
      SqlField("id", GhostDynamicTable.Id, key = true, hidden = true),
      SqlObject("exposureTime"),
      SqlField("exposureCount", channel.ExposureCount),
      SqlField("binning",       channel.Binning),
      SqlField("readMode",      channel.ReadMode)
    )

  lazy val GhostDynamicMapping: ObjectMapping =
    ObjectMapping(GhostDynamicType)(
      SqlField("id", GhostDynamicTable.Id, key = true, hidden = true),
      SqlObject("red"),
      SqlObject("blue"),
      SqlField("ifu1FiberAgitator", GhostDynamicTable.FiberAgitator.Ifu1),
      SqlField("ifu2FiberAgitator", GhostDynamicTable.FiberAgitator.Ifu2),
      CirceField("centralWavelength", CentralWavelength.asJson)
    )

  lazy val GhostDynamicMappings: List[TypeMapping] =
    List(
      detectorMapping(GhostDynamicTable.Blue),
      detectorMapping(GhostDynamicTable.Red),
      GhostDynamicMapping
    )