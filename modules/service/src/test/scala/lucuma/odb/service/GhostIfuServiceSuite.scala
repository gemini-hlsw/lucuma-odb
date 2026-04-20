// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.IO
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import grackle.Result
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

  private val SetObservingMode: Command[(Instrument, ObservingModeType, Observation.Id)] =
    sql"""
      UPDATE t_observation
         SET c_instrument          = $instrument,
             c_observing_mode_type = $observing_mode_type
       WHERE c_observation_id = $observation_id
    """.command

  private def insert(
    in:  GhostIfuInput.Create,
    etm: Option[ExposureTimeMode],
    oid: Observation.Id
  ): IO[Result[Unit]] =
    withServices(serviceUser): services =>
      services
        .transactionally:
          val setType  = services.session.execute(SetObservingMode)(Instrument.Ghost, ObservingModeType.GhostIfu, oid).void
          val doInsert = ghostIfuService.insert(in, etm, List(oid))
          setType *> doInsert

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
      resolutionMode       = in.resolutionMode,
      red                  = DetectorConfig.Red(expectedDetector(in.red, GhostReadMode.Medium)),
      blue                 = DetectorConfig.Blue(expectedDetector(in.blue, GhostReadMode.Slow)),
      explicitIfu1Agitator = in.explicitIfu1FiberAgitator,
      explicitIfu2Agitator = in.explicitIfu2FiberAgitator
    )

  private def roundTrip(
    in:  GhostIfuInput.Create,
    etm: Option[ExposureTimeMode],
    oid: Observation.Id
  ): IO[Unit] =
    assertIO(insert(in, etm, oid) *> select(oid), expected(in, etm))

  test("round trip"):
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
      explicitIfu1FiberAgitator = GhostIfu1FiberAgitator.Enabled.some,
      explicitIfu2FiberAgitator = GhostIfu2FiberAgitator.Disabled.some
    )

    for
      p <- createProgramAs(pi)
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationAs(pi, p, t)
      _ <- roundTrip(create, none, o)
    yield ()

  test("round trip defaults"):
    val etm = ExposureTimeMode.TimeAndCountMode(
      1.secondTimeSpan,
      PosInt.unsafeFrom(1),
      Wavelength.unsafeFromIntPicometers(500001)
    )

    val create = GhostIfuInput.Create(
      resolutionMode = GhostResolutionMode.Standard,
      red            = none,
      blue           = none,
      explicitIfu1FiberAgitator = none,
      explicitIfu2FiberAgitator = none
    )

    for
      p <- createProgramAs(pi)
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationAs(pi, p, t)
      _ <- roundTrip(create, etm.some, o)
    yield ()