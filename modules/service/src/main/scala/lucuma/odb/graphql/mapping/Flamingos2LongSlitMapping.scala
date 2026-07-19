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
import lucuma.core.enums.Flamingos2Decker
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2ReadoutMode
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.*

trait Flamingos2LongSlitMapping[F[_]]
  extends Flamingos2LongSlitView[F]
     with ExposureTimeModeMapping[F]
     with OptionalFieldMapping[F]
     with SlitTelescopeConfigsMapping[F]
     with Predicates[F] { this: SkunkMapping[F] =>

  lazy val Flamingos2LongSlitAcquisitionMapping: ObjectMapping =
    ObjectMapping(Flamingos2LongSlitAcquisitionType)(
      SqlField("observationId", Flamingos2LongSlitView.ObservationId, key = true, hidden = true),

      explicitOrElseDefault[Flamingos2Filter]("filter", "explicitFilter", "defaultFilter"),
      SqlField("defaultFilter",  Flamingos2LongSlitView.AcquisitionFilterDefault),
      SqlField("explicitFilter", Flamingos2LongSlitView.AcquisitionFilter),

      SqlObject("exposureTimeMode", Join(Flamingos2LongSlitView.ObservationId, ExposureTimeModeView.ObservationId))
    )

  lazy val Flamingos2LongSlitMapping: ObjectMapping =
    ObjectMapping(Flamingos2LongSlitType)(

      SqlField("observationId", Flamingos2LongSlitView.ObservationId, key = true, hidden = true),

      SqlField("disperser", Flamingos2LongSlitView.Disperser),
      SqlField("filter",    Flamingos2LongSlitView.Filter),
      SqlField("fpu",       Flamingos2LongSlitView.Fpu),

      SqlObject("exposureTimeMode", Join(Flamingos2LongSlitView.ObservationId, ExposureTimeModeView.ObservationId)),

      SqlField("explicitReadMode", Flamingos2LongSlitView.ReadMode),
      SqlField("explicitReads", Flamingos2LongSlitView.Reads),

      explicitOrElseDefault[Flamingos2Decker]("decker", "explicitDecker", "defaultDecker"),
      SqlField("defaultDecker",  Flamingos2LongSlitView.DeckerDefault),
      SqlField("explicitDecker", Flamingos2LongSlitView.Decker),

      explicitOrElseDefault[Flamingos2ReadoutMode]("readoutMode", "explicitReadoutMode", "defaultReadoutMode"),
      SqlField("defaultReadoutMode",  Flamingos2LongSlitView.ReadoutModeDefault),
      SqlField("explicitReadoutMode", Flamingos2LongSlitView.ReadoutMode),

      // Raw columns (hidden) backing the telescope config cursor fields.
      SqlField("slitOffsetModeEffRaw", Flamingos2LongSlitView.SlitOffsetModeEffective,  hidden = true),
      SqlField("tcEffRaw",             Flamingos2LongSlitView.TelescopeConfigsEffective, hidden = true),
      SqlField("slitOffsetModeDefRaw", Flamingos2LongSlitView.SlitOffsetModeDefault,     hidden = true),
      SqlField("tcDefRaw",             Flamingos2LongSlitView.TelescopeConfigsDefault,   hidden = true),
      SqlField("slitOffsetModeExpRaw", Flamingos2LongSlitView.SlitOffsetMode,            hidden = true),
      SqlField("tcExpRaw",             Flamingos2LongSlitView.TelescopeConfigs,          hidden = true),

      slitTelescopeConfigsField("telescopeConfigs",        "slitOffsetModeEffRaw", "tcEffRaw"),
      slitTelescopeConfigsField("defaultTelescopeConfigs", "slitOffsetModeDefRaw", "tcDefRaw"),
      explicitSlitTelescopeConfigsField("explicitTelescopeConfigs", "slitOffsetModeExpRaw", "tcExpRaw"),

      SqlJson("telluricType", Flamingos2LongSlitView.TelluricType),

      SqlObject("acquisition"),

      SqlField("initialDisperser", Flamingos2LongSlitView.InitialDisperser),
      SqlField("initialFilter",    Flamingos2LongSlitView.InitialFilter),
      SqlField("initialFpu",       Flamingos2LongSlitView.InitialFpu),

    )

  lazy val Flamingos2LongSlitElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (Flamingos2LongSlitAcquisitionType, "exposureTimeMode", Nil) =>
      Elab.transformChild: child =>
        Unique(
          Filter(
            Predicates.exposureTimeMode.role.eql(ExposureTimeModeRole.Acquisition),
            child
          )
        )

    case (Flamingos2LongSlitType, "exposureTimeMode", Nil) =>
      Elab.transformChild: child =>
        Unique(
          Filter(
            Predicates.exposureTimeMode.role.eql(ExposureTimeModeRole.Science),
            child
          )
        )

}
