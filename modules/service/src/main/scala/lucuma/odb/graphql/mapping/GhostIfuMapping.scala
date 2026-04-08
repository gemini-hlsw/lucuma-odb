// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Result
import grackle.skunk.SkunkMapping
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostIfu1FiberAgitator
import lucuma.core.enums.GhostIfu2FiberAgitator
import lucuma.core.enums.GhostReadMode
import lucuma.odb.graphql.table.ExposureTimeModeView
import lucuma.odb.graphql.table.GhostIfuTable



trait GhostIfuMapping[F[_]]
  extends GhostIfuTable[F]
     with ExposureTimeModeView[F]
     with OptionalFieldMapping[F] { this: SkunkMapping[F] =>

  def ghostDetectorConfigMapping(camera: GhostIfuTable.Camera): ObjectMapping =
    ObjectMapping(GhostIfuType / camera.name)(
      SqlField(s"observationId", GhostIfuTable.ObservationId, key = true, hidden = true),
      SqlObject("exposureTimeMode", Join(camera.ExposureTimeModeId, ExposureTimeModeView.Id)),

      explicitOrElseDefault[GhostBinning]("binning", "explicitBinning", "defaultBinning"),
      SqlField("explicitBinning", camera.Binning),
      SqlField("defaultBinning", camera.BinningDefault),

      explicitOrElseDefault[GhostReadMode]("readMode", "explicitReadMode", "defaultReadMode"),
      SqlField("explicitReadMode", camera.ReadMode),
      SqlField("defaultReadMode", camera.ReadModeDefault)
    )

  lazy val RedMapping: ObjectMapping  = ghostDetectorConfigMapping(GhostIfuTable.Red)
  lazy val BlueMapping: ObjectMapping = ghostDetectorConfigMapping(GhostIfuTable.Blue)

  lazy val GhostIfuMapping: ObjectMapping =
    ObjectMapping(GhostIfuType)(
      SqlField("observationId", GhostIfuTable.ObservationId, key = true, hidden = true),
      SqlField("resolutionMode", GhostIfuTable.ResolutionMode),
      SqlObject("red"),
      SqlObject("blue"),

      explicitOrElseDefault[GhostIfu1FiberAgitator]("ifu1Agitator", "explicitIfu1Agitator", "defaultIfu1Agitator"),
      CursorField("defaultIfu1Agitator", _ => Result.success(GhostIfu1FiberAgitator.Disabled)),
      SqlField("explicitIfu1Agitator", GhostIfuTable.Ifu1FiberAgitator),

      explicitOrElseDefault[GhostIfu2FiberAgitator]("ifu2Agitator", "explicitIfu2Agitator", "defaultIfu2Agitator"),
      CursorField("defaultIfu2Agitator", _ => Result.success(GhostIfu2FiberAgitator.Disabled)),
      SqlField("explicitIfu2Agitator", GhostIfuTable.Ifu2FiberAgitator)
    )

  lazy val GhostIfuMappings: List[TypeMapping] =
    List(
      RedMapping,
      BlueMapping,
      GhostIfuMapping
    )

}