// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.EphemerisKeyType
import lucuma.core.model.EphemerisKey
import lucuma.odb.graphql.binding.*

object NonsiderealInput {

  val EphemerisKeyTypeBinding = enumeratedBinding[EphemerisKeyType]

  val Binding: Matcher[EphemerisKey] =
    ObjectFieldsBinding.rmap {
      case List(
        EphemerisKeyTypeBinding.Option("keyType", rKeyType),
        NonEmptyStringBinding.Option("des", rDes),
        NonEmptyStringBinding.Option("key", rKey)
      ) =>
        (rKeyType, rDes, rKey).parTupled.flatMap {

          case (Some(k), Some(d), None) =>
            EphemerisKey.fromTypeAndDes.getOption((k, d.value)) match {
              case Some(k) => Result(k)
              case None    => Matcher.validationFailure(s"Invalid designation '$d' for key type $k.")
            }
          case (None, None, Some(k)) =>
            EphemerisKey.fromString.getOption(k.value) match {
              case Some(k) => Result(k)
              case None    => Matcher.validationFailure(s"Invalid ephemeris key: '$k'.")
            }
          case _ =>
            Matcher.validationFailure(s"You must either provide key, or provide both keyType and des.")

        }
    }
}

