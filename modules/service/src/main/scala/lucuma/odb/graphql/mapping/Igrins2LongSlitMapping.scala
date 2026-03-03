// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Query.Binding
import grackle.Query.Filter
import grackle.Query.Unique
import grackle.QueryCompiler.Elab
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import lucuma.core.enums.Igrins2OffsetMode
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.*

trait Igrins2LongSlitMapping[F[_]]
  extends Igrins2LongSlitView[F]
     with ExposureTimeModeMapping[F]
     with OptionalFieldMapping[F]
     with Predicates[F] { this: SkunkMapping[F] =>

  lazy val Igrins2LongSlitMapping: ObjectMapping =
    ObjectMapping(Igrins2LongSlitType)(

      SqlField("observationId", Igrins2LongSlitView.ObservationId, key = true, hidden = true),

      SqlObject("exposureTimeMode", Join(Igrins2LongSlitView.ObservationId, ExposureTimeModeView.ObservationId)),

      explicitOrElseDefault[Igrins2OffsetMode]("offsetMode", "explicitOffsetMode", "defaultOffsetMode"),
      SqlField("defaultOffsetMode",  Igrins2LongSlitView.OffsetModeDefault),
      SqlField("explicitOffsetMode", Igrins2LongSlitView.OffsetMode),

      explicitOrElseDefault[Boolean]("saveSVCImages", "explicitSaveSVCImages", "defaultSaveSVCImages"),
      SqlField("defaultSaveSVCImages",  Igrins2LongSlitView.SaveSVCImagesDefault),
      SqlField("explicitSaveSVCImages", Igrins2LongSlitView.SaveSVCImages),

    )

  lazy val Igrins2LongSlitElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (Igrins2LongSlitType, "exposureTimeMode", Nil) =>
      Elab.transformChild: child =>
        Unique(
          Filter(
            Predicates.exposureTimeMode.role.eql(ExposureTimeModeRole.Science),
            child
          )
        )

}
