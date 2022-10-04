// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import edu.gemini.grackle.Mapping
import eu.timepit.refined.types.numeric.NonNegBigDecimal
import eu.timepit.refined.types.numeric.NonNegLong
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Encoder
import io.circe.Json
import io.circe.refined._
import lucuma.core.enums.CloudExtinction
import lucuma.core.enums.EphemerisKeyType
import lucuma.core.enums.FocalPlane
import lucuma.core.enums.ImageQuality
import lucuma.core.enums.ObsActiveStatus
import lucuma.core.enums.ObsStatus
import lucuma.core.enums.ScienceMode
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.SpectroscopyCapabilities
import lucuma.core.enums.ToOActivation
import lucuma.core.enums.WaterVapor
import lucuma.core.math.Epoch
import lucuma.core.model.IntPercent
import lucuma.core.model.Observation
import lucuma.core.model.Partner
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.data.EditType
import lucuma.odb.data.Existence
import lucuma.odb.data.PosAngleConstraintMode
import lucuma.odb.data.ProgramUserRole
import lucuma.odb.data.Tag
import lucuma.odb.data.Timestamp
import lucuma.odb.data.UserType

trait LeafMappings[F[_]] extends BaseMapping[F] {

  private given io.circe.Encoder[Epoch] =
    e => Json.fromString(Epoch.fromString.reverseGet(e))

  lazy val LeafMappings: List[TypeMapping] =
    List(
      LeafMapping[BigDecimal](BigDecimalType),
      LeafMapping[CloudExtinction](CloudExtinctionType),
      LeafMapping[EditType](EditTypeType),
      LeafMapping[EphemerisKeyType](EphemerisKeyTypeType),
      LeafMapping[Epoch](EpochStringType),
      LeafMapping[Existence](ExistenceType),
      LeafMapping[Tag](FilterTypeType),
      LeafMapping[FocalPlane](FocalPlaneType),
      LeafMapping[ImageQuality](ImageQualityType),
      LeafMapping[IntPercent](IntPercentType),
      LeafMapping[Long](LongType),
      LeafMapping[NonEmptyString](NonEmptyStringType),
      LeafMapping[NonNegBigDecimal](NonNegBigDecimalType),
      LeafMapping[NonNegLong](NonNegLongType),
      LeafMapping[ObsActiveStatus](ObsActiveStatusType),
      LeafMapping[ObsActiveStatus](ObsActiveStatusType),
      LeafMapping[Observation.Id](ObservationIdType),
      LeafMapping[ObsStatus](ObsStatusType),
      LeafMapping[ObsStatus](ObsStatusType),
      LeafMapping[Tag](PartnerType),
      LeafMapping[PosAngleConstraintMode](PosAngleConstraintModeType),
      LeafMapping[PosBigDecimal](PosBigDecimalType),
      LeafMapping[PosInt](PosIntType),
      LeafMapping[Program.Id](ProgramIdType),
      LeafMapping[ProgramUserRole](ProgramUserRoleType),
      LeafMapping[ScienceMode](ScienceModeType),
      LeafMapping[SkyBackground](SkyBackgroundType),
      LeafMapping[SpectroscopyCapabilities](SpectroscopyCapabilitiesType),
      LeafMapping[String](DmsStringType),
      LeafMapping[String](HmsStringType),
      LeafMapping[Tag](TacCategoryType),
      LeafMapping[Target.Id](TargetIdType),
      LeafMapping[Timestamp](TimestampType),
      LeafMapping[ToOActivation](ToOActivationType),
      LeafMapping[User.Id](UserIdType),
      LeafMapping[UserType](UserTypeType),
      LeafMapping[WaterVapor](WaterVaporType)
    )

}

