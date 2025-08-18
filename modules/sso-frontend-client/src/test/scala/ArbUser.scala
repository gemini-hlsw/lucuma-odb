// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import lucuma.core.model.*
import lucuma.core.model.arb.ArbOrcidProfile.given
import lucuma.core.util.arb.ArbGid.given
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbUser {
  import ArbStandardRole.given

  val GenGuestUser: Gen[GuestUser] =
    arbitrary[User.Id].map(GuestUser(_))

  val GenServiceUser: Gen[ServiceUser] =
    for {
      id <- arbitrary[User.Id]
      n  <- arbitrary[String]
    } yield ServiceUser(id, n)

  val GenStandardUser: Gen[StandardUser] =
    for {
      id <- arbitrary[User.Id]
      r  <- arbitrary[StandardRole]
      rs <- arbitrary[List[StandardRole]].map(_.distinct)
      p  <- arbitrary[OrcidProfile]
    } yield StandardUser(id, r, rs, p)


  implicit val ArbUser: Arbitrary[User] =
    Arbitrary {
      Gen.oneOf(GenGuestUser, GenServiceUser, GenStandardUser)
    }

  implicit val CogUser: Cogen[User] =
    null

}

object ArbUser extends ArbUser
