// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Query.Binding
import grackle.Query.Filter
import grackle.Query.Unique
import grackle.QueryCompiler.Elab
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.*

trait ScorpioLongSlitMapping[F[_]]
  extends ScorpioLongSlitView[F]
     with ExposureTimeModeMapping[F]
     with SlitTelescopeConfigsMapping[F]
     with Predicates[F] { this: SkunkMapping[F] =>

  lazy val ScorpioLongSlitMapping: ObjectMapping =
    ObjectMapping(ScorpioLongSlitType)(
      SqlField("observationId", ScorpioLongSlitView.ObservationId, key = true, hidden = true),

      SqlField("fpu", ScorpioLongSlitView.Fpu),

      SqlObject("g", Join(ScorpioLongSlitView.ObservationId, ScorpioGExposureTimeModeView.ObservationId)),
      SqlObject("r", Join(ScorpioLongSlitView.ObservationId, ScorpioRExposureTimeModeView.ObservationId)),
      SqlObject("i", Join(ScorpioLongSlitView.ObservationId, ScorpioIExposureTimeModeView.ObservationId)),
      SqlObject("z", Join(ScorpioLongSlitView.ObservationId, ScorpioZExposureTimeModeView.ObservationId)),

      SqlField("slitOffsetModeEffRaw", ScorpioLongSlitView.SlitOffsetModeEffective,   hidden = true),
      SqlField("tcEffRaw",             ScorpioLongSlitView.TelescopeConfigsEffective, hidden = true),
      SqlField("slitOffsetModeDefRaw", ScorpioLongSlitView.SlitOffsetModeDefault,     hidden = true),
      SqlField("tcDefRaw",             ScorpioLongSlitView.TelescopeConfigsDefault,   hidden = true),
      SqlField("slitOffsetModeExpRaw", ScorpioLongSlitView.SlitOffsetMode,            hidden = true),
      SqlField("tcExpRaw",             ScorpioLongSlitView.TelescopeConfigs,          hidden = true),

      slitTelescopeConfigsField("telescopeConfigs",        "slitOffsetModeEffRaw", "tcEffRaw"),
      slitTelescopeConfigsField("defaultTelescopeConfigs", "slitOffsetModeDefRaw", "tcDefRaw"),
      explicitSlitTelescopeConfigsField("explicitTelescopeConfigs", "slitOffsetModeExpRaw", "tcExpRaw")
    )

  lazy val ScorpioLongSlitElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (ScorpioLongSlitType, "g" | "r" | "i" | "z", Nil) =>
      Elab.transformChild: child =>
        Unique(
          Filter(
            Predicates.exposureTimeMode.role.eql(ExposureTimeModeRole.Science),
            child
          )
        )

}
