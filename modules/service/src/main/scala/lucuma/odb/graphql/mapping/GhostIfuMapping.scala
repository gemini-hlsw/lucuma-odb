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
import lucuma.odb.graphql.table.GhostIfuView



trait GhostIfuMapping[F[_]]
  extends GhostIfuView[F]
     with ExposureTimeModeView[F]
     with OptionalFieldMapping[F]
     with Predicates[F] { this: SkunkMapping[F] =>

  def ghostDetectorConfigMapping(
    detector: GhostIfuView.DetectorTable,
    etmView:  BaseExposureTimeModeView
  ): ObjectMapping =
    ObjectMapping(GhostIfuType / detector.name)(
      SqlField("observationId", GhostIfuView.ObservationId, key = true, hidden = true),

      SqlObject("exposureTimeMode", Join(GhostIfuView.ObservationId, etmView.ObservationId)),

      explicitOrElseDefault[GhostBinning]("binning", "explicitBinning", "defaultBinning"),
      SqlField("explicitBinning", detector.Binning),
      SqlField("defaultBinning", detector.BinningDefault),

      explicitOrElseDefault[GhostReadMode]("readMode", "explicitReadMode", "defaultReadMode"),
      SqlField("explicitReadMode", detector.ReadMode),
      SqlField("defaultReadMode", detector.ReadModeDefault)
    )

  lazy val GhostIfuMapping: ObjectMapping =
    ObjectMapping(GhostIfuType)(
      SqlField("observationId", GhostIfuView.ObservationId, key = true, hidden = true),
      SqlField("stepCount",      GhostIfuView.StepCount),
      SqlField("resolutionMode", GhostIfuView.ResolutionMode),

      SqlObject("red"),
      SqlObject("blue"),

      SqlObject("slitViewingCameraExposureTime"),

      explicitOrElseDefault[GhostIfu1FiberAgitator]("ifu1Agitator", "explicitIfu1Agitator", "defaultIfu1Agitator"),
      CursorField("defaultIfu1Agitator", _ => Result.success(GhostIfu1FiberAgitator.Disabled)),
      SqlField("explicitIfu1Agitator", GhostIfuView.Ifu1FiberAgitator),

      explicitOrElseDefault[GhostIfu2FiberAgitator]("ifu2Agitator", "explicitIfu2Agitator", "defaultIfu2Agitator"),
      CursorField("defaultIfu2Agitator", _ => Result.success(GhostIfu2FiberAgitator.Disabled)),
      SqlField("explicitIfu2Agitator", GhostIfuView.Ifu2FiberAgitator)
    )

  lazy val GhostIfuMappings: List[TypeMapping] =
    List(
      ghostDetectorConfigMapping(GhostIfuView.Blue, GhostBlueExposureTimeModeView),
      ghostDetectorConfigMapping(GhostIfuView.Red,  GhostRedExposureTimeModeView),
      GhostIfuMapping
    )

  lazy val GhostIfuElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (GhostDetectorConfigType, "exposureTimeMode", Nil) =>
      Elab.transformChild: child =>
        Unique(
          Filter(
            Predicates.exposureTimeMode.role.eql(ExposureTimeModeRole.Science),
            child
          )
        )

}