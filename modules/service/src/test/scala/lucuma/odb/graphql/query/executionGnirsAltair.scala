// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.effect.Ref
import cats.syntax.all.*
import io.circe.literal.*
import lucuma.core.enums.FieldLens
import lucuma.core.math.Angle
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.itc.AltairParameters
import lucuma.itc.ItcVersions
import lucuma.itc.client.ClientCalculationResult
import lucuma.itc.client.ImagingInput
import lucuma.itc.client.InstrumentMode
import lucuma.itc.client.ItcClient
import lucuma.itc.client.SpectroscopyInput
import lucuma.itc.client.SpectroscopyIntegrationTimeAndGraphsInput
import lucuma.itc.client.SpectroscopyIntegrationTimeAndGraphsResult
import lucuma.odb.service.ItcService
import lucuma.odb.service.Services
import lucuma.odb.service.Services.ServiceAccess
import lucuma.odb.util.Codecs.*
import skunk.implicits.*

/**
 * What the generator makes of an Altair configuration: the parameters the ITC is actually called
 * with, and what a change of guide star does to the cached result. The AOWFS patrol field is only
 * a few tens of arcseconds across, so these share `guideEnvironmentGnirsAltair`'s candidate table.
 */
class executionGnirsAltair extends ExecutionTestSupportForGnirs
                                 with GuideEnvironmentSuite:

  override val gaiaResponseString: String = GaiaVoTables.altairCandidates

  override def createObservationAs(user: User, pid: Program.Id, tids: List[Target.Id]): IO[Observation.Id] =
    createGnirsLongSlitObservationAs(user, pid, tids*)

  // Where the fixture star sits relative to the science target, and the R estimated from its Gaia
  // G, BP and RP.  See guideEnvironmentGnirsAltair.
  private val StarSeparation: Angle       = Angle.fromDoubleArcseconds(218.122)
  private val StarRBrightness: BigDecimal = BigDecimal("13.941")

  private def setAltair(oid: Observation.Id, altair: String): IO[Unit] =
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

  private def observationWithAltair(altair: String, guideStar: Boolean): IO[(Program.Id, Observation.Id)] =
    for
      p <- createProgramAs(pi)
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationAs(pi, p, List(t))
      _ <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
      _ <- setAltair(o, altair)
      _ <- IO.whenA(guideStar)(setGuideTargetName(pi, o, defaultTargetName.some))
    yield (p, o)

  private def withItcService[A](client: ItcClient[IO])(f: ServiceAccess ?=> ItcService[IO] => IO[A]): IO[A] =
    withServicesForObscalc(serviceUser): services =>
      given Services[IO] = services
      f(ItcService.instantiate[IO](client))

  /** The standard fake client, recording the instrument mode of every call it is given. */
  private def recordingItcClient(modes: Ref[IO, List[InstrumentMode]]): ItcClient[IO] =
    val delegate: ItcClient[IO] = itcClient
    new ItcClient[IO]:
      override def spectroscopy(input: SpectroscopyInput, useCache: Boolean): IO[ClientCalculationResult] =
        modes.update(input.mode :: _) *> delegate.spectroscopy(input, useCache)

      override def imaging(input: ImagingInput, useCache: Boolean): IO[ClientCalculationResult] =
        modes.update(input.mode :: _) *> delegate.imaging(input, useCache)

      override def spectroscopyIntegrationTimeAndGraphs(
        input:    SpectroscopyIntegrationTimeAndGraphsInput,
        useCache: Boolean
      ): IO[SpectroscopyIntegrationTimeAndGraphsResult] =
        delegate.spectroscopyIntegrationTimeAndGraphs(input, useCache)

      override def versions: IO[ItcVersions] =
        delegate.versions

  /**
   * The Altair parameters the remote ITC is called with, for the acquisition (imaging) calls and
   * for the science (spectroscopy) calls. `callRemote` is used rather than `lookup` so that the
   * cache cannot stand in for the calls being observed.
   */
  private def altairItcParameters(
    pid: Program.Id,
    oid: Observation.Id
  ): IO[(List[Option[AltairParameters]], List[Option[AltairParameters]])] =
    for
      params <- withServices(pi): s =>
                  s.transactionally:
                    s.generatorParamsService
                     .selectOne(pid, oid)
                     .flatMap(_.leftMap(e => new RuntimeException(s"unexpected error: ${e.format}")).liftTo[IO])
      modes  <- IO.ref(List.empty[InstrumentMode])
      _      <- withItcService(recordingItcClient(modes))(_.callRemote(pid, oid, params))
      called <- modes.get
    yield (
      called.collect { case InstrumentMode.GnirsImaging(altair = a)      => a },
      called.collect { case InstrumentMode.GnirsSpectroscopy(altair = a) => a }
    )

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

  private def assertCalled(acquisition: List[Option[AltairParameters]], science: List[Option[AltairParameters]]): Unit =
    assert(acquisition.nonEmpty, "expected at least one acquisition ITC call")
    assert(science.nonEmpty, "expected at least one science ITC call")

  test("LGS+P1 needs no guide star and reaches the ITC"):
    for
      (pid, oid) <- observationWithAltair("{ mode: LGS_P1 }", guideStar = false)
      (acq, sci) <- altairItcParameters(pid, oid)
    yield
      assertCalled(acq, sci)
      (acq ++ sci).foreach(assertEquals(_, AltairParameters.LgsP1.some))

  test("NGS with a stored guide star reaches the ITC with its separation, R and field lens"):
    for
      (pid, oid) <- observationWithAltair("{ mode: NGS }", guideStar = true)
      (acq, sci) <- altairItcParameters(pid, oid)
    yield
      assertCalled(acq, sci)
      (acq ++ sci).foreach(assertNgs(_, FieldLens.In))

  test("NGS without a stored guide star runs the ITC without Altair"):
    for
      (pid, oid) <- observationWithAltair("{ mode: NGS }", guideStar = false)
      (acq, sci) <- altairItcParameters(pid, oid)
    yield
      assertCalled(acq, sci)
      (acq ++ sci).foreach(assertEquals(_, none))

  /** The input hash of the cached ITC result, if there is one. */
  private def itcResultHash(oid: Observation.Id): IO[Option[String]] =
    withSession: s =>
      s.option(
        sql"""
          SELECT c_hash
          FROM t_itc_result
          WHERE c_observation_id = $observation_id
        """.query(md5_hash)
      )(oid).map(_.map(_.toHex))

  test("setting a guide star runs the ITC again for the new star"):
    for
      (pid, oid) <- observationWithAltair("{ mode: NGS }", guideStar = false)
      _          <- runObscalcUpdate(pid, oid)
      before     <- itcResultHash(oid)
      _          <- setGuideTargetName(pi, oid, defaultTargetName.some)
      after      <- itcResultHash(oid)
    yield
      assert(before.isDefined, "the ITC result should be cached before the guide star is set")
      assert(after.isDefined,  "setting the guide star should leave a fresh ITC result")
      assertNotEquals(before, after)

  test("clearing the guide star evicts the cached ITC result"):
    for
      (pid, oid) <- observationWithAltair("{ mode: NGS }", guideStar = true)
      _          <- runObscalcUpdate(pid, oid)
      before     <- itcResultHash(oid)
      _          <- setGuideTargetName(pi, oid, none)
      after      <- itcResultHash(oid)
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

  // The guide star hash folds in the generator hash, which folds in the ITC result, which now
  // depends on the stored star. It stays valid only because the name is stored before the
  // generator runs; see GuideService.setGuideTargetNameImpl.
  test("the stored guide star stays valid once set"):
    observationWithAltair("{ mode: NGS }", guideStar = true).flatMap: (_, oid) =>
      expect(
        pi,
        guideTargetNameQuery(oid),
        expected = json"""
          {
            "observation": {
              "targetEnvironment": {
                "guideTargetName": $defaultTargetName
              }
            }
          }
        """.asRight
      )
