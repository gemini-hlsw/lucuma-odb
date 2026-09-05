// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb

import cats.effect.IO
import cats.effect.Resource
import cats.effect.std.Semaphore
import cats.syntax.all.*
import lucuma.core.data.EmailAddress
import lucuma.core.enums.ExchangePartner
import lucuma.core.enums.GeminiCallForProposalsType
import lucuma.core.enums.Partner
import munit.CatsEffectSuite

/**
 * Tests for the `PROPOSAL_EMAIL_*` configuration. `Config.envOrProp` falls back to system
 * properties, so the values can be set here without touching the environment. Note that a real
 * environment variable takes precedence over a system property, so the tests are skipped
 * entirely when any `PROPOSAL_EMAIL_*` variable is set in the environment (as it is in the nix
 * dev shell).
 */
class ProposalEmailsConfigSuite extends CatsEffectSuite {

  private val Suffixes: List[String] =
    List(
      "DEMO_SCIENCE", "DIRECTORS_TIME", "FAST_TURNAROUND", "LARGE_PROGRAM", "POOR_WEATHER",
      "SYSTEM_VERIFICATION", "SUBARU", "KECK", "AR", "BR", "CA", "CL", "KR", "UH", "US"
    )

  private val sem = ResourceSuiteLocalFixture(
    "semaphore",
    Resource.eval(Semaphore[IO](1L))
  )

  override def munitFixtures = List(sem)

  private def key(suffix: String): String =
    s"PROPOSAL_EMAIL_$suffix"

  private def address(suffix: String): EmailAddress =
    EmailAddress.unsafeFrom(s"${suffix.toLowerCase}@gemini.edu")

  private def putSystemProperty(key: String, value: String): IO[Unit] =
    IO(System.getProperties().put(key, value)).void

  private def removeSystemProperty(key: String): IO[Unit] =
    IO(System.getProperties().remove(key)).void

  private val reset: IO[Unit] =
    (key("DEFAULT") :: Suffixes.map(key)).traverse_(removeSystemProperty)

  private def withProperties(props: (String, String)*)(use: IO[Unit]): IO[Unit] =
    // Not in an IO, so that munit sees the assumption violation and skips the test.
    assume(!sys.env.keys.exists(_.startsWith("PROPOSAL_EMAIL_")), "PROPOSAL_EMAIL_* set in environment")
    sem().permit.use: _ =>
      (reset *> props.toList.traverse_(putSystemProperty) *> use).guarantee(reset)

  private val Default: EmailAddress =
    EmailAddress.unsafeFrom("default@gemini.edu")

  test("all specific variables set"):
    withProperties(Suffixes.map(s => (key(s), address(s).value.value))*):
      Config.ProposalEmails.fromCiris.load[IO].map: pe =>
        assertEquals(pe.forCfpType(GeminiCallForProposalsType.DemoScience),        address("DEMO_SCIENCE").some)
        assertEquals(pe.forCfpType(GeminiCallForProposalsType.DirectorsTime),      address("DIRECTORS_TIME").some)
        assertEquals(pe.forCfpType(GeminiCallForProposalsType.FastTurnaround),     address("FAST_TURNAROUND").some)
        assertEquals(pe.forCfpType(GeminiCallForProposalsType.LargeProgram),       address("LARGE_PROGRAM").some)
        assertEquals(pe.forCfpType(GeminiCallForProposalsType.PoorWeather),        address("POOR_WEATHER").some)
        assertEquals(pe.forCfpType(GeminiCallForProposalsType.SystemVerification), address("SYSTEM_VERIFICATION").some)
        assertEquals(pe.forCfpType(GeminiCallForProposalsType.RegularSemester),    none)
        assertEquals(pe.forExchangePartner(ExchangePartner.Keck),                  address("KECK"))
        assertEquals(pe.forExchangePartner(ExchangePartner.Subaru),                address("SUBARU"))
        Partner.values.toList.foreach: p =>
          assertEquals(pe.forPartner(p), address(p.abbreviation))

  test("default only"):
    withProperties((key("DEFAULT"), Default.value.value)):
      Config.ProposalEmails.fromCiris.load[IO].map: pe =>
        assertEquals(pe, Config.ProposalEmails.uniform(Default))

  test("specific variable overrides the default"):
    withProperties(
      (key("DEFAULT"), Default.value.value),
      (key("KECK"),    address("KECK").value.value)
    ):
      Config.ProposalEmails.fromCiris.load[IO].map: pe =>
        assertEquals(pe.forExchangePartner(ExchangePartner.Keck),   address("KECK"))
        assertEquals(pe.forExchangePartner(ExchangePartner.Subaru), Default)
        assertEquals(pe.forPartner(Partner.US),                     Default)

  test("nothing set is an error"):
    withProperties():
      Config.ProposalEmails.fromCiris.load[IO].attempt.map: e =>
        assert(e.isLeft, "Expected the configuration load to fail.")

  test("malformed address is an error, even with a default"):
    withProperties(
      (key("DEFAULT"), Default.value.value),
      (key("KECK"),    "not an email address")
    ):
      Config.ProposalEmails.fromCiris.load[IO].attempt.map: e =>
        assert(e.isLeft, "Expected the configuration load to fail.")

}
