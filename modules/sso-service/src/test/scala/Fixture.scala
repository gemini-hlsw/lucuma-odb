// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service

import cats.effect.IO
import cats.implicits.*
import lucuma.core.model.*
import lucuma.core.model.StandardRole.*
import lucuma.sso.service.orcid.*
import org.http4s.implicits.*
import weaver.SimpleMutableIOSuite

trait Fixture { self: SimpleMutableIOSuite =>

  val SsoRoot     = uri"https://sso.gpp.lucuma.xyz"
  val ExploreRoot = SsoRoot // uri"https://explore.lucuma.xyz"

  val Alice: OrcidPerson =
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

  val Bob: OrcidPerson =
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
          _ <- expect(Option(last)  === p.name.familyName).failFast
          _ <- expect(Option(first) === p.name.givenName).failFast
          _ <- expect(p.emails.find(_.primary).exists(e => Option(e.email) === email)).failFast
        } yield ()
      case _ => expect(false).failFast
    }

}