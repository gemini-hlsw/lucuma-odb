// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.literal.*
import lucuma.core.enums.FieldLens
import lucuma.core.math.Angle
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.model.sequence.ExecutionDigest
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
import skunk.implicits.*

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
    for
      p <- createProgramAs(pi)
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationAs(pi, p, List(t))
      _ <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
      _ <- setAltair(o, altair)
      _ <- guideStar.traverse_(n => setGuideTargetName(pi, o, n.some))
      _ <- clearItcCalls
    yield (p, o)

  /** Generates the sequence, which is where the Altair guide star loop lives. */
  protected def digestFor(oid: Observation.Id): IO[Either[OdbError, ExecutionDigest]] =
    withServices(serviceUser): services =>
      Services.asSuperUser(services.generator.digest(oid))

class executionGnirsAltair extends AltairItcRecording:

  override val gaiaResponseString: String = GaiaVoTables.altairCandidates

  // The only candidate inside the AOWFS patrol field, where it sits relative to the science target
  // and the R estimated from its Gaia G, BP and RP.  See guideEnvironmentGnirsAltair.
  private val aowfsStarName: String       = "Gaia DR3 3219118090462917888"
  private val StarSeparation: Angle       = Angle.fromDoubleArcseconds(12.0)
  private val StarRBrightness: BigDecimal = BigDecimal("11.736")

  private def assertNgs(altair: Option[AltairParameters], expectedFieldLens: FieldLens): Unit =
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

  test("LGS+P1 needs no guide star and reaches the ITC"):
    for
      (_, oid)   <- observationWithAltair("{ mode: LGS_P1 }", none)
      _          <- digestFor(oid)
      (acq, sci) <- itcAltairCalls
    yield
      assert(acq.nonEmpty && sci.nonEmpty, "expected acquisition and science ITC calls")
      (acq ++ sci).foreach(assertEquals(_, AltairParameters.LgsP1.some))

  test("NGS runs Altair-free first, then again with the resolved guide star"):
    for
      (_, oid)   <- observationWithAltair("{ mode: NGS }", none)
      _          <- digestFor(oid)
      (acq, sci) <- itcAltairCalls
    yield
      assertEquals(acq.headOption, none.some, "the first acquisition call should be Altair-free")
      assertEquals(sci.headOption, none.some, "the first science call should be Altair-free")
      assertNgs(acq.last, FieldLens.In)
      assertNgs(sci.last, FieldLens.In)

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

  test("obscalc calls the remote ITC once per generation pass"):
    for
      (pid, oid) <- observationWithAltair("{ mode: NGS }", aowfsStarName.some)
      _          <- clearItcCalls
      _          <- runObscalcUpdate(pid, oid)
      (_, sci)   <- itcAltairCalls
    yield
      // The Altair-free pass and the one with the resolved guide star, and nothing else: obscalc's
      // own lookup is the first of the two, not a third call.
      assertEquals(sci.length, 2, s"expected one science call per pass, found $sci")
      assertEquals(sci.head, none, "the first pass should be Altair-free")
      assert(sci.last.isDefined, "the second pass should carry the resolved guide star")

  test("setting a guide star runs the ITC again for the new star"):
    for
      (pid, oid) <- observationWithAltair("{ mode: NGS }", none)
      _          <- runObscalcUpdate(pid, oid)
      before     <- itcResultHashes(oid)
      _          <- clearItcCalls
      _          <- setGuideTargetName(pi, oid, aowfsStarName.some)
      (acq, sci) <- itcAltairCalls
      after      <- itcResultHashes(oid)
    yield
      assert(before.isDefined, "the ITC result should be cached before the guide star is set")
      assert(after.isDefined,  "setting the guide star should leave a fresh ITC result")
      assert(acq.nonEmpty && sci.nonEmpty, "setting the guide star should call the ITC again")

  test("clearing the guide star evicts the cached ITC result"):
    for
      (pid, oid) <- observationWithAltair("{ mode: NGS }", aowfsStarName.some)
      _          <- runObscalcUpdate(pid, oid)
      before     <- itcResultHashes(oid)
      _          <- setGuideTargetName(pi, oid, none)
      after      <- itcResultHashes(oid)
    yield
      assert(before.isDefined, "the ITC result should be cached before the guide star is cleared")
      assertEquals(after, none)

  private def guideTargetNameQuery(oid: Observation.Id): String =
    s"""
      query {
        observation(observationId: "$oid") {
          targetEnvironment { guideTargetName }
        }
      }
    """

  // The guide star hash folds in the generator hash, which behind Altair is the one of the
  // Altair-free first pass precisely so that it does not depend on the star being checked; see
  // GeneratorContext.guideStarHash.
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
      (_, oid) <- observationWithAltair("{ mode: LGS_P1 }", aowfsStarName.some)
      digest   <- digestFor(oid)
    yield assert(
      digest.left.exists(_.message.contains("no longer usable")),
      s"expected an unusable guide star error, found $digest"
    )

// With a star inside the 1 arcsecond radius available, AGS prefers it and NGS drops the field lens.
class executionGnirsAltairNearStar extends AltairItcRecording:

  override val gaiaResponseString: String = GaiaVoTables.altairCandidatesWithNearStar

  test("the field lens follows the separation of the resolved guide star"):
    for
      (_, oid)   <- observationWithAltair("{ mode: NGS }", none)
      _          <- digestFor(oid)
      (acq, sci) <- itcAltairCalls
    yield (acq.last :: sci.last :: Nil).foreach:
      case Some(AltairParameters.Ngs(fieldLens = f)) => assertEquals(f, FieldLens.Out)
      case other                                     => fail(s"expected Altair NGS parameters, found $other")

// The default candidate table is built for the PWFS patrol field; none of its stars lies inside
// the much smaller AOWFS one, so AGS has nothing to offer Altair.
class executionGnirsAltairNoStar extends AltairItcRecording:

  test("an Altair observation with no usable guide star fails the sequence"):
    for
      (_, oid) <- observationWithAltair("{ mode: NGS }", none)
      digest   <- digestFor(oid)
    yield assert(
      digest.left.exists(_.message.contains("none is usable")),
      s"expected a missing guide star error, found $digest"
    )
