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
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.*

trait Igrins2LongSlitMapping[F[_]]
  extends Igrins2LongSlitView[F]
     with ExposureTimeModeMapping[F]
     with OptionalFieldMapping[F]
     with SlitTelescopeConfigsMapping[F]
     with Predicates[F] { this: SkunkMapping[F] =>

  val defaultSaveSVCImages: FieldMapping = CursorField[Boolean]("defaultSaveSVCImages", _ => Result(false))

  lazy val Igrins2LongSlitMapping: ObjectMapping =
    ObjectMapping(Igrins2LongSlitType)(

      SqlField("observationId", Igrins2LongSlitView.ObservationId, key = true, hidden = true),

      SqlObject("exposureTimeMode", Join(Igrins2LongSlitView.ObservationId, ExposureTimeModeView.ObservationId)),

      explicitOrElseDefault[Boolean]("saveSVCImages", "explicitSaveSVCImages", "defaultSaveSVCImages"),

      SqlField("explicitSaveSVCImages", Igrins2LongSlitView.SaveSVCImages),
      defaultSaveSVCImages,

      SqlField("slitOffsetModeEffRaw", Igrins2LongSlitView.SlitOffsetModeEffective,  hidden = true),
      SqlField("tcEffRaw",             Igrins2LongSlitView.TelescopeConfigsEffective, hidden = true),
      SqlField("slitOffsetModeDefRaw", Igrins2LongSlitView.SlitOffsetModeDefault,     hidden = true),
      SqlField("tcDefRaw",             Igrins2LongSlitView.TelescopeConfigsDefault,   hidden = true),
      SqlField("slitOffsetModeExpRaw", Igrins2LongSlitView.SlitOffsetMode,            hidden = true),
      SqlField("tcExpRaw",             Igrins2LongSlitView.TelescopeConfigs,          hidden = true),

      slitTelescopeConfigsField("telescopeConfigs",        "slitOffsetModeEffRaw", "tcEffRaw"),
      slitTelescopeConfigsField("defaultTelescopeConfigs", "slitOffsetModeDefRaw", "tcDefRaw"),
      explicitSlitTelescopeConfigsField("explicitTelescopeConfigs", "slitOffsetModeExpRaw", "tcExpRaw"),

      SqlJson("telluricType", Igrins2LongSlitView.TelluricType)

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
