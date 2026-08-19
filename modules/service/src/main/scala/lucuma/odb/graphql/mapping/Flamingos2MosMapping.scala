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

trait Flamingos2MosMapping[F[_]]
  extends Flamingos2MosView[F]
     with ExposureTimeModeMapping[F]
     with OptionalFieldMapping[F]
     with SlitTelescopeConfigsMapping[F]
     with Predicates[F] { this: SkunkMapping[F] =>

  lazy val Flamingos2MosAcquisitionMapping: ObjectMapping =
    ObjectMapping(Flamingos2MosAcquisitionType)(
      SqlField("observationId", Flamingos2MosView.ObservationId, key = true, hidden = true),

      explicitOrElseDefault[Flamingos2Filter]("filter", "explicitFilter", "defaultFilter"),
      SqlField("defaultFilter",  Flamingos2MosView.AcquisitionFilterDefault),
      SqlField("explicitFilter", Flamingos2MosView.AcquisitionFilter),

      SqlObject("exposureTimeMode", Join(Flamingos2MosView.ObservationId, ExposureTimeModeView.ObservationId))
    )

  lazy val Flamingos2MosCustomMaskMapping: ObjectMapping =
    ObjectMapping(Flamingos2MosType / "customMask")(
      SqlField("observationId", Flamingos2MosView.ObservationId, key = true, hidden = true),
      SqlField("slitWidth",     Flamingos2MosView.SlitWidth),
      SqlField("attachmentId",  Flamingos2MosView.MaskAttachmentId)
    )

  lazy val Flamingos2MosMapping: ObjectMapping =
    ObjectMapping(Flamingos2MosType)(

      SqlField("observationId", Flamingos2MosView.ObservationId, key = true, hidden = true),

      SqlField("disperser", Flamingos2MosView.Disperser),
      SqlField("filter",    Flamingos2MosView.Filter),

      SqlObject("customMask"),

      SqlObject("exposureTimeMode", Join(Flamingos2MosView.ObservationId, ExposureTimeModeView.ObservationId)),

      SqlField("explicitReadMode", Flamingos2MosView.ReadMode),
      SqlField("explicitReads",    Flamingos2MosView.Reads),

      explicitOrElseDefault[Flamingos2Decker]("decker", "explicitDecker", "defaultDecker"),
      SqlField("defaultDecker",  Flamingos2MosView.DeckerDefault),
      SqlField("explicitDecker", Flamingos2MosView.Decker),

      explicitOrElseDefault[Flamingos2ReadoutMode]("readoutMode", "explicitReadoutMode", "defaultReadoutMode"),
      SqlField("defaultReadoutMode",  Flamingos2MosView.ReadoutModeDefault),
      SqlField("explicitReadoutMode", Flamingos2MosView.ReadoutMode),

      SqlField("offsetPreset", Flamingos2MosView.OffsetPreset),

      // Raw columns (hidden) backing the telescope config cursor fields.
      SqlField("slitOffsetModeEffRaw", Flamingos2MosView.SlitOffsetModeEffective,  hidden = true),
      SqlField("tcEffRaw",             Flamingos2MosView.TelescopeConfigsEffective, hidden = true),
      SqlField("slitOffsetModeDefRaw", Flamingos2MosView.SlitOffsetModeDefault,     hidden = true),
      SqlField("tcDefRaw",             Flamingos2MosView.TelescopeConfigsDefault,   hidden = true),
      SqlField("slitOffsetModeExpRaw", Flamingos2MosView.SlitOffsetMode,            hidden = true),
      SqlField("tcExpRaw",             Flamingos2MosView.TelescopeConfigs,          hidden = true),

      slitTelescopeConfigsField("telescopeConfigs",        "slitOffsetModeEffRaw", "tcEffRaw"),
      slitTelescopeConfigsField("defaultTelescopeConfigs", "slitOffsetModeDefRaw", "tcDefRaw"),
      explicitSlitTelescopeConfigsField("explicitTelescopeConfigs", "slitOffsetModeExpRaw", "tcExpRaw"),

      SqlJson("telluricType", Flamingos2MosView.TelluricType),

      SqlObject("acquisition"),

      // Read-only snapshot of what the mode was created with
      SqlField("initialDisperser", Flamingos2MosView.InitialDisperser),
      SqlField("initialFilter",    Flamingos2MosView.InitialFilter),
      SqlField("initialSlitWidth", Flamingos2MosView.InitialSlitWidth)
    )

  lazy val Flamingos2MosElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (Flamingos2MosAcquisitionType, "exposureTimeMode", Nil) =>
      Elab.transformChild: child =>
        Unique(
          Filter(
            Predicates.exposureTimeMode.role.eql(ExposureTimeModeRole.Acquisition),
            child
          )
        )

    case (Flamingos2MosType, "exposureTimeMode", Nil) =>
      Elab.transformChild: child =>
        Unique(
          Filter(
            Predicates.exposureTimeMode.role.eql(ExposureTimeModeRole.Science),
            child
          )
        )

  lazy val Flamingos2MosMappings: List[TypeMapping] =
    List(
      Flamingos2MosMapping,
      Flamingos2MosAcquisitionMapping,
      Flamingos2MosCustomMaskMapping
    )

}
