// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all.*
import io.circe.literal.*
import lucuma.core.enums.FieldLens
import lucuma.core.math.Angle
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.util.TimeSpan
import lucuma.itc.AltairParameters
import lucuma.itc.ItcVersions
import lucuma.itc.client.ClientCalculationResult
import lucuma.itc.client.ImagingInput
import lucuma.itc.client.InstrumentMode
import lucuma.itc.client.ItcClient
import lucuma.itc.client.SpectroscopyInput
import lucuma.itc.client.SpectroscopyIntegrationTimeAndGraphsInput
import lucuma.itc.client.SpectroscopyIntegrationTimeAndGraphsResult
import lucuma.odb.data.Md5Hash
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.syntax.hash.*
import lucuma.odb.service.Services
import lucuma.odb.util.Codecs.*
import org.http4s.Request
import org.http4s.Response
import skunk.implicits.*

import java.io.IOException
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReference

/**
 * What the generator makes of an Altair configuration: the parameters the ITC is actually called
 * with, once the guide star the observation will use has been resolved, and what a change of guide
 * star does to the cached result. The AOWFS patrol field is only a few tens of arcseconds across,
 * so these share `guideEnvironmentGnirsAltair`'s candidate table.
 */
trait AltairItcRecording extends ExecutionTestSupportForGnirs with GuideEnvironmentSuite:

  override def createObservationAs(user: User, pid: Program.Id, tids: List[Target.Id]): IO[Observation.Id] =
    createGnirsLongSlitObservationAs(user, pid, tids*)

  // Every instrument mode the remote ITC has been called with, oldest first.
  private val calls: AtomicReference[List[InstrumentMode]] =
    new AtomicReference(List.empty)

  override protected def itcClient: ItcClient[IO] =
    val delegate: ItcClient[IO] = super.itcClient
    new ItcClient[IO]:
      override def spectroscopy(input: SpectroscopyInput, useCache: Boolean): IO[ClientCalculationResult] =
        IO(calls.updateAndGet(_ :+ input.mode)) *> delegate.spectroscopy(input, useCache)

      override def imaging(input: ImagingInput, useCache: Boolean): IO[ClientCalculationResult] =
        IO(calls.updateAndGet(_ :+ input.mode)) *> delegate.imaging(input, useCache)

      override def spectroscopyIntegrationTimeAndGraphs(
        input:    SpectroscopyIntegrationTimeAndGraphsInput,
        useCache: Boolean
      ): IO[SpectroscopyIntegrationTimeAndGraphsResult] =
        delegate.spectroscopyIntegrationTimeAndGraphs(input, useCache)

      override def versions: IO[ItcVersions] =
        delegate.versions

  protected def clearItcCalls: IO[Unit] =
    IO(calls.set(List.empty))

  /** The Altair parameters of the acquisition (imaging) and science (spectroscopy) ITC calls. */
  protected def itcAltairCalls: IO[(List[Option[AltairParameters]], List[Option[AltairParameters]])] =
    IO(calls.get).map: modes =>
      (
        modes.collect { case InstrumentMode.GnirsImaging(altair = a)      => a },
        modes.collect { case InstrumentMode.GnirsSpectroscopy(altair = a) => a }
      )

  protected def setAltair(oid: Observation.Id, altair: String): IO[Unit] =
    query(
      user  = pi,
      query = s"""
        mutation {
          updateObservations(input: {
            WHERE: { id: { EQ: "$oid" } }
            SET: { targetEnvironment: { altair: $altair } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  protected def observationWithAltair(altair: String, guideStar: Option[String]): IO[(Program.Id, Observation.Id)] =
    observationWithAltair(altair, guideStar, fullTimeEstimate.some)

  protected def observationWithAltair(
    altair:      String,
    guideStar:   Option[String],
    obsDuration: Option[TimeSpan]
  ): IO[(Program.Id, Observation.Id)] =
    for
      p <- createProgramAs(pi)
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationAs(pi, p, List(t))
      _ <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, obsDuration)
      _ <- setAltair(o, altair)
      // Setting the guide star generates the sequence, and that generation is what the tests of a
      // stored star look at, so the recording starts before it.
      _ <- clearItcCalls
      _ <- guideStar.traverse_(n => setGuideTargetName(pi, o, n.some))
    yield (p, o)

  /** Generates the sequence, which is where the Altair guide star loop lives. */
  protected def digestFor(oid: Observation.Id): IO[Either[OdbError, ExecutionDigest]] =
    withServices(serviceUser): services =>
      Services.asSuperUser(services.generator.digest(oid))

  // The only candidate inside the AOWFS patrol field of the default table, where it sits relative
  // to the science target and the R estimated from its Gaia G, BP and RP.  See
  // guideEnvironmentGnirsAltair.
  protected val aowfsStarName: String       = "Gaia DR3 3219118090462917888"
  protected val StarSeparation: Angle       = Angle.fromDoubleArcseconds(12.0)
  protected val StarRBrightness: BigDecimal = BigDecimal("11.736")

  protected def assertNgs(altair: Option[AltairParameters], expectedFieldLens: FieldLens): Unit =
    altair match
      case Some(AltairParameters.Ngs(separation, brightness, fieldLens)) =>
        assert(
          (Angle.signedDecimalArcseconds.get(separation) - Angle.signedDecimalArcseconds.get(StarSeparation)).abs < BigDecimal("0.1"),
          s"unexpected separation $separation"
        )
        assert((brightness.value.value - StarRBrightness).abs < BigDecimal("0.01"), s"unexpected R magnitude $brightness")
        assertEquals(fieldLens, expectedFieldLens)
      case other                                                        =>
        fail(s"expected Altair NGS parameters, found $other")

class executionGnirsAltair extends AltairItcRecording:

  override val gaiaResponseString: String = GaiaVoTables.altairCandidates

  test("LGS+P1 needs no guide star and reaches the ITC"):
    for
      (_, oid)   <- observationWithAltair("{ mode: LGS_P1 }", none)
      _          <- digestFor(oid)
      (acq, sci) <- itcAltairCalls
    yield
      assert(acq.nonEmpty && sci.nonEmpty, "expected acquisition and science ITC calls")
      (acq ++ sci).foreach(assertEquals(_, AltairParameters.LgsP1.some))

  // The star is picked from the database before anything is generated, so the ITC never sees an
  // Altair-free configuration: the requested signal to noise may well be out of reach without
  // Altair, which would make such a call meaningless.
  test("NGS sends the resolved guide star on the first ITC call"):
    for
      (_, oid)   <- observationWithAltair("{ mode: NGS }", none)
      _          <- digestFor(oid)
      (acq, sci) <- itcAltairCalls
    yield
      assertEquals(sci.length, 1, s"expected a single science call, found $sci")
      assertNgs(sci.head, FieldLens.In)
      assertNgs(acq.head, FieldLens.In)

  test("an observation without a stored duration picks with the nominal visit duration"):
    for
      (_, oid)   <- observationWithAltair("{ mode: NGS }", none, none)
      digest     <- digestFor(oid)
      (acq, sci) <- itcAltairCalls
    yield
      assert(digest.isRight, s"expected a digest, found $digest")
      assertNgs(sci.head, FieldLens.In)
      assertNgs(acq.head, FieldLens.In)

  test("a stored guide star reaches the ITC with its separation, R and field lens"):
    for
      (_, oid)   <- observationWithAltair("{ mode: NGS }", aowfsStarName.some)
      _          <- digestFor(oid)
      (acq, sci) <- itcAltairCalls
    yield
      assertNgs(acq.last, FieldLens.In)
      assertNgs(sci.last, FieldLens.In)

  /** The input and Altair hashes of the cached ITC result, if there is one. */
  private def itcResultHashes(oid: Observation.Id): IO[Option[(String, Option[String])]] =
    withSession: s =>
      s.option(
        sql"""
          SELECT c_hash, c_altair_hash
          FROM t_itc_result
          WHERE c_observation_id = $observation_id
        """.query(md5_hash *: md5_hash.opt)
      )(oid).map(_.map((hash, altair) => (hash.toHex, altair.map(_.toHex))))

  /** The ITC input hash the database alone yields: the one every reader but the generator computes. */
  private def starlessInputHash(pid: Program.Id, oid: Observation.Id): IO[Option[String]] =
    withServices(pi): services =>
      services.transactionally:
        services.generatorParamsService.selectOne(pid, oid).map: params =>
          params.toOption.flatMap(_.itcInput.toOption).map(i => Md5Hash.unsafeFromByteArray(i.md5).toHex)

  private def validationMessages(oid: Observation.Id): IO[List[String]] =
    query(
      user  = pi,
      query = s"""
        query {
          observation(observationId: "$oid") {
            workflow { value { validationErrors { messages } } }
          }
        }
      """
    ).map: json =>
      val value = json.hcursor.downFields("observation", "workflow", "value")
      assert(value.focus.exists(!_.isNull), s"expected a calculated workflow, found $json")
      value
        .downField("validationErrors")
        .values
        .toList
        .flatten
        .flatMap(_.hcursor.downField("messages").require[List[String]])

  test("the cached ITC result keys on the star-less input hash, Altair apart"):
    for
      (pid, oid) <- observationWithAltair("{ mode: NGS }", aowfsStarName.some)
      _          <- runObscalcUpdate(pid, oid)
      hashes     <- itcResultHashes(oid)
      starless   <- starlessInputHash(pid, oid)
    yield hashes match
      case Some((hash, altair)) =>
        assertEquals(hash.some, starless, "the cached result should key on the star-less input hash")
        assert(altair.isDefined, "the Altair parameters should be keyed alongside it")
      case None                 =>
        fail("expected a cached ITC result")

  test("the workflow finds the ITC result of an Altair observation"):
    for
      (pid, oid) <- observationWithAltair("{ mode: NGS }", aowfsStarName.some)
      _          <- runObscalcUpdate(pid, oid)
      messages   <- validationMessages(oid)
    yield assert(
      !messages.exists(_.contains("ITC results are not present")),
      s"the workflow should see the cached ITC result, but reported $messages"
    )

  test("obscalc calls the remote ITC once for a settled observation"):
    for
      (pid, oid) <- observationWithAltair("{ mode: NGS }", none)
      _          <- clearItcCalls
      _          <- runObscalcUpdate(pid, oid)
      (_, sci)   <- itcAltairCalls
    yield
      // Obscalc has no lookup of its own any more: it takes the result the generation produced,
      // which is the only one computed with the guide star the observation will use.
      assertEquals(sci.length, 1, s"expected a single science call, found $sci")
      assert(sci.head.isDefined, "the only call should carry the resolved guide star")

  // The Altair parameters are keyed apart on the cached result, so a change of guide star needs
  // no eviction: the next generation simply does not reuse a result computed for another star.
  // A reader has only the database, so it cannot compute the Altair half of the key. Were it to
  // compare one anyway it would miss every Altair result, call the remote service Altair-free, and
  // overwrite the result the sequence is generated from.
  test("reading the ITC of an Altair observation leaves the cached result alone"):
    for
      (pid, oid) <- observationWithAltair("{ mode: NGS }", aowfsStarName.some)
      _          <- runObscalcUpdate(pid, oid)
      before     <- itcResultHashes(oid)
      _          <- clearItcCalls
      _          <- query(pi, s"""query { observation(observationId: "$oid") { itc { itcType } } }""")
      after      <- itcResultHashes(oid)
      (acq, sci) <- itcAltairCalls
    yield
      assert(before.exists(_._2.isDefined), "the cached result should carry Altair parameters")
      assertEquals(after, before)
      assertEquals(acq ++ sci, Nil, "reading the ITC should reach no remote call")

  // The acquisition is re-derived from the parameters the science side generates with, guide star
  // included; reading them back from the database would lose Altair.
  test("resetting the acquisition re-derives it with the resolved guide star"):
    for
      (_, oid) <- observationWithAltair("{ mode: NGS }", aowfsStarName.some)
      _        <- digestFor(oid)
      _        <- clearItcCalls
      _        <- resetAcquisitionAs(serviceUser, oid)
      (acq, _) <- itcAltairCalls
    yield
      assert(acq.nonEmpty, "the reset should call the acquisition ITC")
      assertNgs(acq.last, FieldLens.In)

  // LGS+P1 guides with PWFS1, whose probe arm would vignette the AOWFS star.
  test("an unusable stored star stops the sequence being materialized"):
    for
      (_, oid) <- observationWithAltair("{ mode: LGS_P1 }", aowfsStarName.some)
      result   <- withServices(serviceUser)(_.generator.materialize(oid))
    yield assert(
      result.left.exists(_.message.contains("no longer usable")),
      s"expected an unusable guide star error, found $result"
    )

  test("clearing the guide star leaves the cached ITC result in place"):
    for
      (pid, oid) <- observationWithAltair("{ mode: NGS }", aowfsStarName.some)
      _          <- runObscalcUpdate(pid, oid)
      before     <- itcResultHashes(oid)
      _          <- setGuideTargetName(pi, oid, none)
      after      <- itcResultHashes(oid)
    yield
      assert(before.isDefined, "the ITC result should be cached before the guide star is cleared")
      assertEquals(after, before)

  private def guideTargetNameQuery(oid: Observation.Id): String =
    s"""
      query {
        observation(observationId: "$oid") {
          targetEnvironment { guideTargetName }
        }
      }
    """

  // The guide star hash folds in the generator hash, which leaves the ITC result out precisely so
  // that it does not depend on the star being checked; see GeneratorContext.guideStarHash.
  test("the stored guide star stays valid once set"):
    observationWithAltair("{ mode: NGS }", aowfsStarName.some).flatMap: (_, oid) =>
      expect(
        pi,
        guideTargetNameQuery(oid),
        expected = json"""
          {
            "observation": {
              "targetEnvironment": {
                "guideTargetName": $aowfsStarName
              }
            }
          }
        """.asRight
      )

  // LGS+P1 guides with PWFS1, whose probe arm would vignette a star as close in as the AOWFS one.
  test("a stored star the current parameters rule out fails the sequence"):
    for
      (_, oid)   <- observationWithAltair("{ mode: LGS_P1 }", aowfsStarName.some)
      _          <- clearItcCalls
      digest     <- digestFor(oid)
      (acq, sci) <- itcAltairCalls
    yield
      assert(
        digest.left.exists(_.message.contains("no longer usable")),
        s"expected an unusable guide star error, found $digest"
      )
      assertEquals(acq ++ sci, Nil, "an unusable stored star should reach no ITC call")

// With a star inside the 1 arcsecond radius available, AGS prefers it and NGS drops the field lens.
class executionGnirsAltairNearStar extends AltairItcRecording:

  override val gaiaResponseString: String = GaiaVoTables.altairCandidatesWithNearStar

  // AGS prefers the near star here, so storing the AOWFS one is a real change of ITC input; with
  // the same star on both sides the cached result would simply be reused.
  test("setting a guide star runs the ITC again for the new star"):
    for
      (pid, oid) <- observationWithAltair("{ mode: NGS }", none)
      _          <- runObscalcUpdate(pid, oid)
      _          <- clearItcCalls
      _          <- setGuideTargetName(pi, oid, aowfsStarName.some)
      (acq, sci) <- itcAltairCalls
    yield
      assert(acq.nonEmpty && sci.nonEmpty, "setting the guide star should call the ITC again")
      assertNgs(sci.last, FieldLens.In)

  test("the field lens follows the separation of the resolved guide star"):
    for
      (_, oid)   <- observationWithAltair("{ mode: NGS }", none)
      _          <- digestFor(oid)
      (acq, sci) <- itcAltairCalls
    yield (acq.last :: sci.last :: Nil).foreach:
      case Some(AltairParameters.Ngs(fieldLens = f)) => assertEquals(f, FieldLens.Out)
      case other                                     => fail(s"expected Altair NGS parameters, found $other")

/**
 * Once execution has begun the digest comes from the frozen ITC result, without resolving the
 * guide star again: the sequence is already materialized, so a fresh AGS pass could only make the
 * two disagree, and Gaia being unreachable would leave an executing observation with no digest.
 */
class executionGnirsAltairFrozen extends AltairItcRecording:

  override val gaiaResponseString: String = GaiaVoTables.altairCandidates

  // When set, every Gaia query fails, as an outage would.
  private val gaiaDown: AtomicBoolean = new AtomicBoolean(false)

  override protected def httpRequestHandler: Request[IO] => Resource[IO, Response[IO]] =
    request =>
      if gaiaDown.get then Resource.eval(IO.raiseError(new IOException("Gaia unavailable")))
      else super.httpRequestHandler(request)

  // Marks the cached result frozen, as the freeze at execution start would.
  private def freezeItcResult(oid: Observation.Id): IO[Unit] =
    withSession: s =>
      s.execute(
        sql"""
          UPDATE t_itc_result
             SET c_is_frozen = true
           WHERE c_observation_id = $observation_id
        """.command
      )(oid).void

  test("a frozen Altair result generates without resolving the guide star again"):
    for
      (_, oid)   <- observationWithAltair("{ mode: NGS }", aowfsStarName.some)
      before     <- digestFor(oid)
      _          <- freezeItcResult(oid)
      _          <- clearItcCalls
      _          <- IO(gaiaDown.set(true))
      after      <- digestFor(oid)
      (acq, sci) <- itcAltairCalls
    yield
      assert(before.isRight, s"expected a digest before the freeze, found $before")
      assertEquals(after, before)
      assertEquals(acq ++ sci, Nil, "a frozen result should reach no remote ITC call")

/**
 * Setting a guide star stores the name before the generator runs and the hash only afterwards, so
 * a generator failure in between must leave the observation exactly as it found it. A name stored
 * without a hash reads as a selection still waiting for its generation, and so would never go
 * stale.
 */
class executionGnirsAltairFailedSet extends AltairItcRecording:

  override val gaiaResponseString: String = GaiaVoTables.altairCandidates

  // When set, every Gaia query fails, as an outage would.
  private val gaiaDown: AtomicBoolean = new AtomicBoolean(false)

  override protected def httpRequestHandler: Request[IO] => Resource[IO, Response[IO]] =
    request =>
      if gaiaDown.get then Resource.eval(IO.raiseError(new IOException("Gaia unavailable")))
      else super.httpRequestHandler(request)

  private def storedGuideStar(oid: Observation.Id): IO[Option[(Option[String], Option[String])]] =
    withSession: s =>
      s.option(
        sql"""
          SELECT c_guide_target_name, c_guide_target_hash
          FROM t_observation
          WHERE c_observation_id = $observation_id
        """.query(guide_target_name.opt *: md5_hash.opt)
      )(oid).map(_.map((name, hash) => (name.map(_.value.value), hash.map(_.toHex))))

  test("a guide star set the generator cannot complete leaves the stored selection alone"):
    for
      (_, oid) <- observationWithAltair("{ mode: NGS }", aowfsStarName.some)
      before   <- storedGuideStar(oid)
      _        <- IO(gaiaDown.set(true))
      failed   <- setGuideTargetName(pi, oid, otherTargetName.some).attempt
      after    <- storedGuideStar(oid)
    yield
      assert(before.exists((name, hash) => name.contains(aowfsStarName) && hash.isDefined), s"expected a stored selection, found $before")
      assert(failed.isLeft, "the mutation should fail while Gaia is unreachable")
      assertEquals(after, before)

// The default candidate table is built for the PWFS patrol field; none of its stars lies inside
// the much smaller AOWFS one, so AGS has nothing to offer Altair.
class executionGnirsAltairNoStar extends AltairItcRecording:

  test("an Altair observation with no usable guide star fails the sequence"):
    for
      (_, oid)   <- observationWithAltair("{ mode: NGS }", none)
      digest     <- digestFor(oid)
      (acq, sci) <- itcAltairCalls
    yield
      assert(
        digest.left.exists(_.message.contains("none is usable")),
        s"expected a missing guide star error, found $digest"
      )
      assertEquals(acq ++ sci, Nil, "a missing guide star should reach no ITC call")
