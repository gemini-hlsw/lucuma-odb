// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import cats.syntax.all.*
import lucuma.core.enums.Partner
import lucuma.core.model.*
import lucuma.core.model.StandardRole.*
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbGid.given
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.cats.implicits.*

trait ArbStandardRole {

  implicit val ArbStandardRole: Arbitrary[StandardRole] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[StandardRole.Id].map(Admin(_)),
        (arbitrary[StandardRole.Id], arbitrary[Partner]).mapN(Ngo(_, _)),
        arbitrary[StandardRole.Id].map(Pi(_)),
        arbitrary[StandardRole.Id].map(Staff(_)),
      )
    }

  implicit val CogStandardRole: Cogen[StandardRole] =
    Cogen[Long].contramap {
      case Admin(id)        => id.value.value
      case Ngo(id, partner) => id.value.value + partner.tag.hashCode()
      case Pi(id)           => id.value.value
      case Staff(id)        => id.value.value
    }

}

object ArbStandardRole extends ArbStandardRole
