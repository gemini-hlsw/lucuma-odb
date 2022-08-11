// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package mapping

import edu.gemini.grackle.Mapping
import eu.timepit.refined.types.numeric.NonNegBigDecimal
import eu.timepit.refined.types.numeric.NonNegLong
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.refined._
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.Existence
import lucuma.odb.data.UserType
import lucuma.odb.data.ProgramUserRole
import lucuma.core.model.Partner
import lucuma.odb.data.ObsStatus
import lucuma.odb.data.ObsActiveStatus
import lucuma.core.model.Observation
import lucuma.core.enums.CloudExtinction
import lucuma.core.enums.ImageQuality
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.enums.EphemerisKeyType
import lucuma.core.model.Target
import io.circe.Encoder
import lucuma.core.math.Epoch
import io.circe.Json
import lucuma.odb.data.Tag

trait LeafMappings[F[_]] { this: Mapping[F] =>

  private given Encoder[Epoch] =
    e => Json.fromString(Epoch.fromString.reverseGet(e))

  lazy val BigDecimalType        = schema.ref("BigDecimal")
  lazy val CloudExtinctionType   = schema.ref("CloudExtinction")
  lazy val DmsStringType         = schema.ref("DmsString")
  lazy val EphemerisKeyTypeType  = schema.ref("EphemerisKeyType")
  lazy val EpochStringType       = schema.ref("EpochString")
  lazy val ExistenceType         = schema.ref("Existence")
  lazy val HmsStringType         = schema.ref("HmsString")
  lazy val HourAngleRangeType    = schema.ref("HourAngleRange")
  lazy val ImageQualityType      = schema.ref("ImageQuality")
  lazy val LongType              = schema.ref("Long")
  lazy val NonEmptyStringType    = schema.ref("NonEmptyString")
  lazy val NonNegBigDecimalType  = schema.ref("NonNegBigDecimal")
  lazy val NonNegLongType        = schema.ref("NonNegLong")
  lazy val ObsActiveStatusType   = schema.ref("ObsActiveStatus")
  lazy val ObservationIdType     = schema.ref("ObservationId")
  lazy val ObsStatusType         = schema.ref("ObsStatus")
  lazy val PartnerType           = schema.ref("Partner")
  lazy val PosBigDecimalType     = schema.ref("PosBigDecimal")
  lazy val ProgramIdType         = schema.ref("ProgramId")
  lazy val ProgramUserRoleType   = schema.ref("ProgramUserRole")
  lazy val SkyBackgroundType     = schema.ref("SkyBackground")
  lazy val UserIdType            = schema.ref("UserId")
  lazy val UserTypeType          = schema.ref("UserType")
  lazy val TargetIdType          = schema.ref("TargetId")
  lazy val WaterVaporType        = schema.ref("WaterVapor")

  lazy val LeafMappings: List[TypeMapping] =
    List(
      LeafMapping[BigDecimal](BigDecimalType),
      LeafMapping[CloudExtinction](CloudExtinctionType),
      LeafMapping[EphemerisKeyType](EphemerisKeyTypeType),
      LeafMapping[Epoch](EpochStringType),
      LeafMapping[Existence](ExistenceType),
      LeafMapping[ImageQuality](ImageQualityType),
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
      LeafMapping[PosBigDecimal](PosBigDecimalType),
      LeafMapping[Program.Id](ProgramIdType),
      LeafMapping[ProgramUserRole](ProgramUserRoleType),
      LeafMapping[SkyBackground](SkyBackgroundType),
      LeafMapping[String](DmsStringType),
      LeafMapping[String](HmsStringType),
      LeafMapping[Target.Id](TargetIdType),
      LeafMapping[User.Id](UserIdType),
      LeafMapping[UserType](UserTypeType),
      LeafMapping[WaterVapor](WaterVaporType)
    )

}

