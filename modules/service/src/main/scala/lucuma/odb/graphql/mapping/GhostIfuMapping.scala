// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Query.Binding
import grackle.Query.Filter
import grackle.Query.Unique
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostIfu1FiberAgitator
import lucuma.core.enums.GhostIfu2FiberAgitator
import lucuma.core.enums.GhostReadMode
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.ExposureTimeModeView
import lucuma.odb.graphql.table.GhostExposureTimeModeLinkView
import lucuma.odb.graphql.table.GhostIfuTable



trait GhostIfuMapping[F[_]]
  extends GhostIfuTable[F]
     with GhostExposureTimeModeLinkView[F]
     with ExposureTimeModeView[F]
     with OptionalFieldMapping[F]
     with Predicates[F] { this: SkunkMapping[F] =>

  def ghostDetectorConfigMapping(
    arm: GhostIfuTable.Arm
  ): ObjectMapping =
    ObjectMapping(GhostIfuType / arm.name / "detector")(
      SqlField("observationId", GhostIfuTable.ObservationId, key = true, hidden = true),

      explicitOrElseDefault[GhostBinning]("binning", "explicitBinning", "defaultBinning"),
      SqlField("explicitBinning", arm.Binning),
      SqlField("defaultBinning", arm.BinningDefault),

      explicitOrElseDefault[GhostReadMode]("readMode", "explicitReadMode", "defaultReadMode"),
      SqlField("explicitReadMode", arm.ReadMode),
      SqlField("defaultReadMode", arm.ReadModeDefault)
    )

  lazy val GhostBlueArmMapping: ObjectMapping =
    ObjectMapping(GhostBlueArmType)(
      SqlField("observationId", GhostIfuTable.ObservationId, key = true, hidden = true),
      SqlObject("exposureTimeMode", Join(GhostIfuTable.ObservationId, GhostExposureTimeModeLinkView.BlueObservationId), Join(GhostExposureTimeModeLinkView.Id, ExposureTimeModeView.Id)),
      SqlObject("detector")
    )

  lazy val GhostRedArmMapping: ObjectMapping =
    ObjectMapping(GhostRedArmType)(
      SqlField("observationId", GhostIfuTable.ObservationId, key = true, hidden = true),
      SqlObject("exposureTimeMode", Join(GhostIfuTable.ObservationId, GhostExposureTimeModeLinkView.RedObservationId), Join(GhostExposureTimeModeLinkView.Id, ExposureTimeModeView.Id)),
      SqlObject("detector")
    )

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
      ghostDetectorConfigMapping(GhostIfuTable.Red),
      ghostDetectorConfigMapping(GhostIfuTable.Blue),
      GhostBlueArmMapping,
      GhostRedArmMapping,
      GhostIfuMapping
    )

  lazy val GhostIfuElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (GhostBlueArmType, "exposureTimeMode", Nil) =>
      Elab.transformChild: child =>
        Unique(
          Filter(
            Predicates.exposureTimeMode.role.eql(ExposureTimeModeRole.Science),
            child
          )
        )

    case (GhostRedArmType, "exposureTimeMode", Nil) =>
      Elab.transformChild: child =>
        Unique(
          Filter(
            Predicates.exposureTimeMode.role.eql(ExposureTimeModeRole.Science),
            child
          )
        )

}