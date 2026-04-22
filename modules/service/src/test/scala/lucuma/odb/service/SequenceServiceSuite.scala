// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.option.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Stream
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostIfu1FiberAgitator
import lucuma.core.enums.GhostIfu2FiberAgitator
import lucuma.core.enums.GhostReadMode
import lucuma.core.enums.GhostResolutionMode.Standard
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.DatasetEstimate
import lucuma.core.model.sequence.DetectorEstimate
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.ghost.GhostDetector
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.ghost.GhostStaticConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos
import lucuma.odb.sequence.data.StreamingExecutionConfig
import lucuma.odb.sequence.gmos.InitialConfigs.GmosNorthStatic
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.Codecs.instrument
import lucuma.odb.util.Codecs.observation_id
import skunk.*
import skunk.implicits.*

import java.util.UUID

class SequenceServiceSuite extends ExecutionTestSupportForGmos:

  override val FakeItcResult: IntegrationTime =
    IntegrationTime(
      10.secTimeSpan,
      PosInt.unsafeFrom(100)
    )

  private def generateSequence(
    o: Observation.Id
  ): IO[List[Atom[GmosNorth]]] =
    withServices(serviceUser): services =>
      services.generator.generate(o).map: e =>
        val g =
          e.toOption
           .flatMap: iec =>
             InstrumentExecutionConfig.gmosNorth.getOption(iec)
           .get.executionConfig.science.get
        g.nextAtom :: g.possibleFuture

  private def readGmosNorthSequence(
    o: Observation.Id
  ): IO[List[Atom[GmosNorth]]] =
    withServices(serviceUser): services =>
      services
        .transactionally:
          sequenceService
            .selectGmosNorthSequence(o, SequenceType.Science, GmosNorthStatic)
            .flatMap(_.toList.flatTraverse(_.compile.toList))

  test("exercise serialization"):
    val res = for
      p  <- createProgram
      t  <- createTargetWithProfileAs(pi, p)
      o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      sn <- generateSequence(o)

      b  <- IO.realTimeInstant
      _  <- recordVisitAs(serviceUser, o)
      e0 <- IO.realTimeInstant
      _  <- recordVisitAs(serviceUser, o)
      e1 <- IO.realTimeInstant

      so <- readGmosNorthSequence(o)
      e2 <- IO.realTimeInstant

//      _  <- IO.println(s"WRITE TIME: ${java.time.Duration.between(b,  e0)}")
//      _  <- IO.println(s"NEW VISIT.: ${java.time.Duration.between(e0, e1)}")
//      _  <- IO.println(s"READ TIME.: ${java.time.Duration.between(e1, e2)}")
//      _  <- IO.println(s"SEQUENCE.n: ${sn.mkString("\n")}")
//      _  <- IO.println("----")
//      _  <- IO.println(s"SEQUENCE.o: ${so.mkString("\n")}")
    yield (sn, so)

    assertIOBoolean(res.map((in, out) => in == out))

  private def setInstrument(i: Instrument, o: Observation.Id): IO[Unit] =
    val up =
      sql"""
        UPDATE t_observation
        SET c_instrument = $instrument
        WHERE c_observation_id = $observation_id
      """.command

    withSession: s =>
      s.execute(up)(i, o).void

  private def readGhostSequence(
    o: Observation.Id
  ): IO[List[Atom[GhostDynamicConfig]]] =
    withServices(serviceUser): services =>
      services
        .transactionally:
          sequenceService
            .selectGhostSequence(o, SequenceType.Science, GhostStaticConfig(Standard, none))
            .flatMap(_.toList.flatTraverse(_.compile.toList))

  private def writeGhostSequence(
    o: Observation.Id,
    c: StreamingExecutionConfig[IO, GhostStaticConfig, GhostDynamicConfig]
  ): IO[Unit] =
    withServices(serviceUser): services =>
      services
        .transactionally:
          sequenceService
            .materializeGhostExecutionConfig(o, c)

  test("ghost - simple round trip"):
    def estimate(c: String, d: GhostDetector, readout: TimeSpan): DetectorEstimate =
      DetectorEstimate(
        s"GHOST $c",
        s"GHOST $c Detector Array",
        DatasetEstimate(
          d.exposureTime,
          readout,
          5.secondTimeSpan
        ),
        NonNegInt.unsafeFrom(d.exposureCount.value)
      )

    val red10 = GhostDetector.Red(
      GhostDetector(
        10.secondTimeSpan,
        PosInt.unsafeFrom(10),
        GhostBinning.TwoByTwo,
        GhostReadMode.Slow
      )
    )
    val red20 = GhostDetector.Red(
      GhostDetector(
        20.secondTimeSpan,
        PosInt.unsafeFrom(20),
        GhostBinning.OneByOne,
        GhostReadMode.Fast
      )
    )
    val blue5 = GhostDetector.Blue(
      GhostDetector(
        5.secondTimeSpan,
        PosInt.unsafeFrom(5),
        GhostBinning.FourByFour,
        GhostReadMode.Medium
      )
    )
    val blue6 = GhostDetector.Blue(
      GhostDetector(
        6.secondTimeSpan,
        PosInt.unsafeFrom(6),
        GhostBinning.OneByTwo,
        GhostReadMode.Slow
      )
    )

    val sid0 = Step.Id.fromUuid(UUID.fromString("BE3F5D25-39D1-40A4-BE21-9E0885477C27"))
    val s0   = Step(
      sid0,
      GhostDynamicConfig(
        red10,
        blue5,
        GhostIfu1FiberAgitator.Enabled,
        GhostIfu2FiberAgitator.Disabled
      ),
      StepConfig.Science,
      TelescopeConfig.Default,
      StepEstimate.fromMax(
        Nil,
        List(
          estimate("Red",  red10.value, 27500.millisecondTimeSpan),
          estimate("Blue", blue5.value,  6500.millisecondTimeSpan)
        )
      )
    )

    val sid1 = Step.Id.fromUuid(UUID.fromString("4AD572C3-A6D2-4ADB-BA0C-AC550075889F"))
    val s1   = Step(
      sid1,
      GhostDynamicConfig(
        red20,
        blue6,
        GhostIfu1FiberAgitator.Disabled,
        GhostIfu2FiberAgitator.Enabled
      ),
      StepConfig.Science,
      TelescopeConfig.Default,
      StepEstimate.fromMax(
        Nil,
        List(
          estimate("Red",  red20.value, 21700.millisecondTimeSpan),
          estimate("Blue", blue6.value, 24800.millisecondTimeSpan)
        )
      )
    )

    val aid0 = Atom.Id.fromUuid(UUID.fromString("24B2D9D6-2F93-47CF-802B-54E5E022CB07"))
    val a0 = Atom(
      aid0,
      NonEmptyString.unsafeFrom("Atom 0").some,
      NonEmptyList.one(s0)
    )
    val aid1 = Atom.Id.fromUuid(UUID.fromString("AD406FE7-9F1E-4080-A8B2-ADBA50B95B15"))
    val a1 = Atom(
      aid1,
      NonEmptyString.unsafeFrom("Atom 1").some,
      NonEmptyList.one(s1)
    )

    val config = StreamingExecutionConfig[IO, GhostStaticConfig, GhostDynamicConfig](
      GhostStaticConfig(Standard, none),
      Stream.empty.covary[IO],
      Stream.emits(List(a0, a1)).covary[IO]
    )

    for
      p <- createProgram
      o <- createObservationAs(pi, p)
      _ <- setInstrument(Instrument.Ghost, o)
      _ <- writeGhostSequence(o, config)
      r <- readGhostSequence(o)
    yield assertEquals(r, List(a0, a1))

