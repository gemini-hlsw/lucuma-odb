// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package igrins2
package longslit

import cats.data.NonEmptyList
import cats.data.State
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepGuideState.Enabled
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2StaticConfig
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.util.AtomBuilder

import java.util.UUID

object Science:

  /**
   * The name of the ABBA cycle atoms.
   */
  val AbbaCycleTitle: NonEmptyString = NonEmptyString.unsafeFrom("ABBA Cycle")

  object Igrins2SequenceState extends SequenceState[Igrins2DynamicConfig]:
    override val initialDynamicConfig: Igrins2DynamicConfig =
      Igrins2DynamicConfig(TimeSpan.Min)

  case class StepDefinition(
    scienceSteps: NonEmptyList[ProtoStep[Igrins2DynamicConfig]]
  ):
    def cycleCount(t: IntegrationTime): NonNegInt =
      val requiredExposures = t.exposureCount.value
      val exposuresPerCycle = scienceSteps.size
      NonNegInt.unsafeFrom((requiredExposures + (exposuresPerCycle - 1)) / exposuresPerCycle)

  object StepDefinition:

    def compute(
      config: Config,
      time:   IntegrationTime
    ): Either[String, StepDefinition] =
      val offsets = config.offsets
      NonEmptyList.fromList(offsets)
        .toRight("At least one offset position is required for IGRINS-2 Long Slit.")
        .map: nel =>
          val sciSteps = Igrins2SequenceState.eval:
            for
              _ <- State.modify[Igrins2DynamicConfig](_.copy(exposure = time.exposureTime))
              ss <- nel.traverse: o =>
                      Igrins2SequenceState.scienceStep(
                        TelescopeConfig(o, Enabled),
                        ObserveClass.Science
                      )
            yield ss
          StepDefinition(sciSteps)

  case class Generator(
    steps:      StepDefinition,
    builder:    AtomBuilder[Igrins2DynamicConfig],
    goalCycles: NonNegInt
  ) extends SequenceGenerator[Igrins2DynamicConfig]:

    override def generate: Stream[Pure, Atom[Igrins2DynamicConfig]] =
      val scienceAtom: ProtoAtom[ProtoStep[Igrins2DynamicConfig]] =
        ProtoAtom(AbbaCycleTitle.some, steps.scienceSteps)

      val atoms: List[ProtoAtom[ProtoStep[Igrins2DynamicConfig]]] =
        List.fill(goalCycles.value)(scienceAtom)

      builder.buildStream(Stream.emits(atoms))

  private def definitionError(oid: Observation.Id, msg: String): OdbError =
    OdbError.SequenceUnavailable(oid, s"Could not generate a sequence for $oid: $msg".some)

  private def zeroExposureTime(oid: Observation.Id): OdbError =
    definitionError(oid, "IGRINS-2 Long Slit requires a positive exposure time.")

  def instantiate(
    observationId: Observation.Id,
    estimator:     TimeEstimateCalculator[Igrins2StaticConfig, Igrins2DynamicConfig],
    static:        Igrins2StaticConfig,
    namespace:     UUID,
    config:        Config,
    time:          Either[OdbError, IntegrationTime]
  ): Either[OdbError, SequenceGenerator[Igrins2DynamicConfig]] =
    for
      t <- time.filterOrElse(
             _.exposureTime.toNonNegMicroseconds.value > 0,
             zeroExposureTime(observationId)
           )
      s <- StepDefinition.compute(config, t)
             .leftMap(m => definitionError(observationId, m))
      c  = s.cycleCount(t)
    yield Generator(
      s,
      AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Science),
      c
    ): SequenceGenerator[Igrins2DynamicConfig]
