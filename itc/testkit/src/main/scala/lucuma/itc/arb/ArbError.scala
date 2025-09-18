// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.arb

import lucuma.itc.Error
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbError:

  given Arbitrary[Error.SourceTooBright] =
    Arbitrary {
      arbitrary[BigDecimal].map(Error.SourceTooBright(_))
    }

  given Cogen[Error.SourceTooBright] =
    Cogen[BigDecimal].contramap(_.wellHalfFilledSeconds)

  given Arbitrary[Error.General] =
    Arbitrary {
      arbitrary[String].map(Error.General(_))
    }

  given Cogen[Error.General] =
    Cogen[String].contramap(_.message)

  given Arbitrary[Error] =
    Arbitrary {
      for {
        sourceTooBright <- arbitrary[Error.SourceTooBright]
        general         <- arbitrary[Error.General]
        e               <- Gen.oneOf(sourceTooBright, general)
      } yield e
    }

  given Cogen[Error] =
    Cogen[Either[BigDecimal, String]].contramap {
      case Error.SourceTooBright(wellHalfFilledSeconds) => Left(wellHalfFilledSeconds)
      case Error.General(message)                       => Right(message)
    }

object ArbError extends ArbError
