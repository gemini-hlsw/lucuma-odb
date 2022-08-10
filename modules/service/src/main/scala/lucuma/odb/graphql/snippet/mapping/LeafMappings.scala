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

trait LeafMappings[F[_]] { this: Mapping[F] =>

  lazy val BigDecimalType       = schema.ref("BigDecimal")
  lazy val DmsStringType        = schema.ref("DmsString")
  lazy val ExistenceType        = schema.ref("Existence")
  lazy val HmsStringType        = schema.ref("HmsString")
  lazy val LongType             = schema.ref("Long")
  lazy val NonEmptyStringType   = schema.ref("NonEmptyString")
  lazy val NonNegBigDecimalType = schema.ref("NonNegBigDecimal")
  lazy val NonNegLongType       = schema.ref("NonNegLong")
  lazy val PosBigDecimalType    = schema.ref("PosBigDecimal")
  lazy val ProgramIdType        = schema.ref("ProgramId")
  lazy val UserIdType           = schema.ref("UserId")
  lazy val UserTypeType         = schema.ref("UserType")

  lazy val LeafMappings: List[TypeMapping] =
    List(
      LeafMapping[BigDecimal](BigDecimalType),
      LeafMapping[Existence](ExistenceType),
      LeafMapping[Long](LongType),
      LeafMapping[NonEmptyString](NonEmptyStringType),
      LeafMapping[NonNegBigDecimal](NonNegBigDecimalType),
      LeafMapping[NonNegLong](NonNegLongType),
      LeafMapping[PosBigDecimal](PosBigDecimalType),
      LeafMapping[Program.Id](ProgramIdType),
      LeafMapping[String](DmsStringType),
      LeafMapping[String](HmsStringType),
      LeafMapping[User.Id](UserIdType),
      LeafMapping[UserType](UserTypeType)
    )

}

