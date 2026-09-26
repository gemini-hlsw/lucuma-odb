// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.graphql

import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import grackle.skunk.SkunkMapping
import io.circe.refined.*
import lucuma.core.enums.*
import lucuma.core.model.ProgramReference
import lucuma.core.model.Semester
import lucuma.core.util.Timestamp
import lucuma.odb.data.Existence
import resource.model.ComponentLocation
import resource.model.InstrumentComponentType
import resource.model.InstrumentPlace
import resource.model.MoonPhase
import resource.model.PowerSource
import resource.model.ResourceInstrument
import resource.model.ResourceUsage
import resource.model.TelescopeAvailability
import resource.model.TelescopeModeType
import resource.model.TelescopeSubsystem
import resource.model.TooSupport

import java.time.LocalDate

trait LeafMappings[F[_]] extends BaseMapping[F]:
  this: SkunkMapping[F] =>

  private given io.circe.Encoder[Semester] =
    lucuma.odb.json.semester.given_Codec_Semester

  lazy val LeafMappings: List[TypeMapping] =
    List(
      LeafMapping[BigDecimal](BigDecimalType),
      LeafMapping[ComponentLocation](ComponentLocationType),
      LeafMapping[LocalDate](DateType),
      LeafMapping[Existence](ExistenceType),
      LeafMapping[InstrumentComponentType](InstrumentComponentTypeType),
      LeafMapping[Long](LongType),
      LeafMapping[MoonPhase](MoonPhaseType),
      LeafMapping[NonEmptyString](NonEmptyStringType),
      LeafMapping[Partner](PartnerType),
      LeafMapping[PosInt](PosIntType),
      LeafMapping[ProgramReference](ProgramReferenceLabelType),
      LeafMapping[Semester](SemesterType),
      LeafMapping[Site](SiteType),
      LeafMapping[TelescopeAvailability](TelescopeAvailabilityType),
      LeafMapping[TelescopeModeType](TelescopeModeType),
      LeafMapping[Timestamp](TimestampType),
      LeafMapping[TooSupport](TooSupportType),
      LeafMapping[ResourceInstrument](ResourceInstrumentType),
      LeafMapping[InstrumentPlace](InstrumentPlaceType),
      LeafMapping[ResourceUsage](ResourceUsageType),
      LeafMapping[TelescopeSubsystem](TelescopeSubsystemType),
      LeafMapping[PowerSource](PowerSourceType)
    )
