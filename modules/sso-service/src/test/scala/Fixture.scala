// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service

import cats.effect.IO
import cats.implicits.*
import lucuma.core.model.*
import lucuma.core.model.StandardRole.*
import lucuma.sso.service.orcid.*
import munit.CatsEffectSuite
import org.http4s.implicits.*

trait Fixture { self: CatsEffectSuite =>

  lazy val SsoRoot     = uri"https://sso.gpp.lucuma.xyz"
  lazy val ExploreRoot = SsoRoot // uri"https://explore.lucuma.xyz"

  lazy val AliceOrcidId = OrcidId.parse("https://orcid.org/1388-2458-9396-3360").toOption.get
  lazy val BobOrcidId = OrcidId.parse("https://orcid.org/7286-2347-4388-1398").toOption.get

  // Some extras to use if we need them
  // lazy val AliceOrcidId = OrcidId.parse("https://orcid.org/0633-2185-7266-6708").toOption.get
  // lazy val AliceOrcidId = OrcidId.parse("https://orcid.org/5422-6667-1666-161X").toOption.get
  // lazy val AliceOrcidId = OrcidId.parse("https://orcid.org/7832-3898-0128-7435").toOption.get

  lazy val Alice: OrcidPerson =
    OrcidPerson(
      name = OrcidName(
        familyName = Some("Dallas"),
        givenName  = Some("Alice"),
        creditName = None
      ),
      emails = List(
        OrcidEmail(
          email    = "alice@dallas.com",
          verified = true,
          primary  = true,
        ),
        OrcidEmail(
          email    = "alice@fnord.com",
          verified = false,
          primary  = false,
        )
      )
    )

  lazy val Bob: OrcidPerson =
    OrcidPerson(
      name = OrcidName(
        familyName = Some("Dobbs"),
        givenName  = Some("Bob"),
        creditName = None
      ),
      emails = List(
        OrcidEmail(
          email    = "bob@dobbs.com",
          verified = true,
          primary  = true,
        ),
        OrcidEmail(
          email    = "chunkmonkey69@aol.com",
          verified = false,
          primary  = false,
        )
      )
    )

  def expectLoggedInAsPi(p: OrcidPerson, u: User): IO[Unit] =
    u match {
      case StandardUser(_, Pi(_), Nil, OrcidProfile(_, UserProfile(Some(first), Some(last), None, email))) =>
        for {
          _ <- IO(Option(last) === p.name.familyName).assert
          _ <- IO(Option(first) === p.name.givenName).assert
          _ <- IO(p.emails.find(_.primary).exists(e => Option(e.email) === email)).assert
        } yield ()
      case _ => fail("Assertion failed")
    }

}