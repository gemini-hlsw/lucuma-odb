// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.EphemerisKeyType
import lucuma.core.model.Ephemeris
import lucuma.odb.graphql.binding.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.data.PerSite

object NonsiderealInput {

  enum Create:
    case Horizons(key: Ephemeris.Key.Horizons)
    case UserSupplied(ephemeris: PerSite[List[Ephemeris.UserSupplied.Element]])

  enum Edit:
    def key: Ephemeris.Key
    case Horizons(key: Ephemeris.Key.Horizons)
    case UserSupplied(key: Ephemeris.Key.UserSupplied, ephemeris: PerSite[List[Ephemeris.UserSupplied.Element]])

  val EphemerisKeyTypeBinding = enumeratedBinding[EphemerisKeyType]
  val EphemerisKeyBinding: Matcher[Ephemeris.Key] =
    NonEmptyStringBinding.rmap: nes =>
      Result.fromOption(Ephemeris.Key.fromString.getOption(nes.toString), s"Invalid ephemeris key: $nes")

  val resolveKey: ((Option[EphemerisKeyType], Option[NonEmptyString], Option[Ephemeris.Key])) => Result[Option[Ephemeris.Key]] =
    case (Some(t), Some(d), None) => Result.fromOption(Ephemeris.Key.fromTypeAndDes.getOption((t, d.toString)), s"Invalid ephemeris key: type=$t, des=$d").map(_.some)
    case (None, None, Some(k))    => Result(k.some)
    case (None, None, None)       => Result(None)
    case _                        => Result.failure("Must specify either (type and designation) or key, but not both.")

  val CreateBinding: Matcher[Create] =
    ObjectFieldsBinding.rmap {
      case List(
        EphemerisKeyTypeBinding.Option("keyType", rKeyType),
        NonEmptyStringBinding.Option("des", rDes),
        EphemerisKeyBinding.Option("key", rKey),
        UserSuppliedEphemerisInput.Binding.Option("ephemeris", rEphemeris),
      ) =>
        val rEphemerisKey = (rKeyType, rDes, rKey).parTupled.flatMap(resolveKey)
        (rEphemerisKey, rEphemeris).parTupled.flatMap:
          case (None, Some(eph)) => Result.success(Create.UserSupplied(eph))
          case (Some(k: Ephemeris.Key.Horizons), None) => Result.success(Create.Horizons(k))
          case (Some(k: Ephemeris.Key.Horizons), Some(_)) => Result.failure("Cannot specify a Horizons key with a user-supplied ephemeris.")
          case (Some(k: Ephemeris.Key.UserSupplied), _) => Result.failure("Cannot specify a user-supplied ephemeris key on creation.")
          case (None, None) => Result.failure("Must specify either a Horizons key or a user-supplied ephemeris, but not both.")
    }

  val EditBinding: Matcher[Edit] =
    ObjectFieldsBinding.rmap {
      case List(
        EphemerisKeyTypeBinding.Option("keyType", rKeyType),
        NonEmptyStringBinding.Option("des", rDes),
        EphemerisKeyBinding.Option("key", rKey),
        UserSuppliedEphemerisInput.Binding.Option("ephemeris", rEphemeris),
      ) =>
        val rEphemerisKey = (rKeyType, rDes, rKey).parTupled.flatMap(resolveKey)
        (rEphemerisKey, rEphemeris).parTupled.flatMap:
          case (Some(k: Ephemeris.Key.Horizons), None) => Result.success(Edit.Horizons(k))
          case (Some(k: Ephemeris.Key.Horizons), Some(_)) => Result.failure("Cannot specify a Horizons key with a user-supplied ephemeris.")
          case (Some(k: Ephemeris.Key.UserSupplied), Some(eph)) => Result.success(Edit.UserSupplied(k, eph))
          case (Some(k: Ephemeris.Key.UserSupplied), None) => Result.failure("Must specify an ephemeris if the key type is user-supplied.")
          case (None, _) => Result.failure("Must specify an ephemeris key on update.")
    }

}

