// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.IO
import cats.syntax.apply.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import grackle.ResultT
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostIfu1FiberAgitator
import lucuma.core.enums.GhostIfu2FiberAgitator
import lucuma.core.enums.GhostReadMode
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservingModeType
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.input.GhostDetectorConfigInput
import lucuma.odb.graphql.input.GhostIfuInput
import lucuma.odb.graphql.query.ExecutionTestSupport
import lucuma.odb.sequence.ghost.DetectorConfig
import lucuma.odb.sequence.ghost.ifu.Config
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.Codecs.instrument
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.observing_mode_type
import skunk.*
import skunk.implicits.*

class GhostIfuServiceSuite extends ExecutionTestSupport:

  val etm = ExposureTimeMode.TimeAndCountMode(
    1.secondTimeSpan,
    PosInt.unsafeFrom(1),
    Wavelength.unsafeFromIntPicometers(500001)
  )

  val redEtm = ExposureTimeMode.TimeAndCountMode(
    1.secondTimeSpan,
    PosInt.unsafeFrom(1),
    Wavelength.unsafeFromIntPicometers(500001)
  )

  val blueEtm = ExposureTimeMode.TimeAndCountMode(
    2.secondTimeSpan,
    PosInt.unsafeFrom(2),
    Wavelength.unsafeFromIntPicometers(500002)
  )

  val create = GhostIfuInput.Create(
    stepCount      = PosInt.unsafeFrom(3),
    resolutionMode = GhostResolutionMode.Standard,
    red            = GhostDetectorConfigInput(
      redEtm.some,
      Nullable.NonNull(GhostBinning.TwoByTwo),
      Nullable.NonNull(GhostReadMode.Fast)
    ).some,
    blue           = GhostDetectorConfigInput(
      blueEtm.some,
      Nullable.NonNull(GhostBinning.FourByFour),
      Nullable.NonNull(GhostReadMode.Slow)
    ).some,
    slitCameraExposureTime    = TimeSpan.FromMicroseconds.getOption(1000L),
    explicitIfu1FiberAgitator = GhostIfu1FiberAgitator.Enabled.some,
    explicitIfu2FiberAgitator = GhostIfu2FiberAgitator.Disabled.some
  )

  private def setObservingMode(
    oid:      Observation.Id,
    inst:     Option[Instrument],
    modeType: Option[ObservingModeType]
  )(using Services[IO]): IO[Unit] =
    val cmd = sql"""
      UPDATE t_observation
         SET c_instrument          = ${instrument.opt},
             c_observing_mode_type = ${observing_mode_type.opt}
       WHERE c_observation_id = $observation_id
    """.command

    services.session.execute(cmd)(inst, modeType, oid).void

  private def insert(
    in:  GhostIfuInput.Create,
    etm: Option[ExposureTimeMode],
    oid: Observation.Id
  ): IO[Unit] =
    withServices(serviceUser): services =>
      services
        .transactionally:
          val setType  = setObservingMode(oid, Instrument.Ghost.some, ObservingModeType.GhostIfu.some)
          val doInsert = ghostIfuService.insert(in, etm, List(oid))
          (ResultT.liftF(setType) *> ResultT(doInsert)).value.flatMap: r =>
            r.toEither match
              case Right(_)        => IO.unit
              case Left(Right(ps)) => IO.raiseError(Exception(ps.toString))
              case Left(Left(e))   => IO.raiseError(e)

  private def select(
    oid: Observation.Id
  ): IO[Config] =
    withServices(serviceUser): services =>
      services
        .transactionally:
          ghostIfuService
            .select(List(oid))
            .map(m => m(oid))

  private def expected(
    in:  GhostIfuInput.Create,
    etm: Option[ExposureTimeMode],
  ): Config =
    def expectedDetector(
      in: Option[GhostDetectorConfigInput],
      rm: GhostReadMode
    ): DetectorConfig =
      val exposureTimeMode = in.flatMap(_.exposureTimeMode).orElse(etm).get
      DetectorConfig(
        exposureTimeMode = exposureTimeMode,
        defaultBinning   = GhostBinning.OneByOne,
        explicitBinning  = in.flatMap(_.explicitBinning.toOption),
        defaultReadMode  = rm,
        explicitReadMode = in.flatMap(_.explicitReadMode.toOption)
      )

    Config(
      stepCount              = in.stepCount,
      resolutionMode         = in.resolutionMode,
      red                    = DetectorConfig.Red(expectedDetector(in.red, GhostReadMode.Medium)),
      blue                   = DetectorConfig.Blue(expectedDetector(in.blue, GhostReadMode.Slow)),
      slitCameraExposureTime = in.slitCameraExposureTime,
      explicitIfu1Agitator   = in.explicitIfu1FiberAgitator,
      explicitIfu2Agitator   = in.explicitIfu2FiberAgitator
    )

  private def roundTrip(
    in:  GhostIfuInput.Create,
    etm: Option[ExposureTimeMode],
    oid: Observation.Id
  ): IO[Unit] =
    assertIO(insert(in, etm, oid) *> select(oid), expected(in, etm))

  test("round trip"):
    for
      p <- createProgramAs(pi)
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationAs(pi, p, t)
      _ <- roundTrip(create, none, o)
    yield ()

  test("round trip defaults"):
    val create = GhostIfuInput.Create(
      stepCount                 = PosInt.unsafeFrom(1),
      resolutionMode            = GhostResolutionMode.Standard,
      red                       = none,
      blue                      = none,
      slitCameraExposureTime    = none,
      explicitIfu1FiberAgitator = none,
      explicitIfu2FiberAgitator = none
    )

    for
      p <- createProgramAs(pi)
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationAs(pi, p, t)
      _ <- roundTrip(create, etm.some, o)
    yield ()

  def expectObservingModeInconsistency(update: Services[IO] => Transaction[IO] ?=> IO[Unit]): IO[Unit] =
    withServices(serviceUser): services =>
      services
        .transactionally:
          update(services)
        .flatMap(_ => IO.raiseError(new Exception("Expected observing mode inconsistency exception but none was raised")))
        .recoverWith:
          case SqlState.RaiseException(ex) if ex.message.contains("Observing mode inconsistency for observation") => IO.unit

  test("delete observing mode only"):
    for
      p <- createProgramAs(pi)
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationAs(pi, p, t)
      _ <- insert(create, none, o)
      _ <- expectObservingModeInconsistency: services =>
            services.ghostIfuService.delete(List(o)) *>
            services.exposureTimeModeService.deleteMany(List(o), ExposureTimeModeRole.values*)

    yield ()

  test("set observing mode type only"):
    for
      p <- createProgramAs(pi)
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationAs(pi, p, t)
      _ <- expectObservingModeInconsistency: services =>
            services.exposureTimeModeService.insertOne(o, ExposureTimeModeRole.Science, ExposureTimeMode.TimeAndCountMode(10.secondTimeSpan, PosInt.unsafeFrom(10), Wavelength.unsafeFromIntPicometers(500000))) *>
            services.exposureTimeModeService.insertOne(o, ExposureTimeModeRole.Science, ExposureTimeMode.TimeAndCountMode(20.secondTimeSpan, PosInt.unsafeFrom(20), Wavelength.unsafeFromIntPicometers(500000))) *>
            setObservingMode(o, Instrument.Ghost.some, ObservingModeType.GhostIfu.some)(using services)
    yield ()