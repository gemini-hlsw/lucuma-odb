// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gnirs
package longslit

import cats.data.NonEmptyList
import cats.data.State
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMirrorMode
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsGratingWavelength
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.itc.IntegrationTime
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.util.AtomBuilder

import java.util.UUID

object Science:

  val ScienceCycleTitle: NonEmptyString =
    NonEmptyString.unsafeFrom("Science Cycle")

  private object SeqState extends gnirs.GnirsSequenceState

  case class StepDefinition(
    scienceSteps: NonEmptyList[ProtoStep[GnirsDynamicConfig]]
  ):
    /**
     * Cycle count: round up so that we always deliver at least the requested
     * number of exposures. Sky-only offsets in the cycle (if any) are not
     * given special treatment — we cycle blindly through whatever the
     * observing mode defines.
     */
    def cycleCount(t: IntegrationTime): NonNegInt =
      val required = t.exposureCount.value
      val perCycle = scienceSteps.length
      NonNegInt.unsafeFrom((required + (perCycle - 1)) / perCycle)

  object StepDefinition:

    def compute(
      config: Config,
      time:   IntegrationTime
    ): StepDefinition =
      // Configure the dynamic config for a science step, then traverse the
      // telescope configs from the observing mode in order, producing one
      // science ProtoStep per offset.
      val resolvedReadMode = config.explicitReadMode.getOrElse(GnirsReadMode.forExposureTime(time.exposureTime))
      val acqMirror        = GnirsAcquisitionMirrorMode.Out(
        config.prism,
        config.grating,
        GnirsGratingWavelength(config.gratingWavelength)
      )

      val sciSteps = SeqState.eval:
        for
          _  <- State.modify[GnirsDynamicConfig]: dyn =>
                  dyn.copy(
                    exposure          = time.exposureTime,
                    coadds            = config.coadds,
                    centralWavelength = config.gratingWavelength,
                    filter            = config.filter,
                    decker            = config.decker,
                    fpu               = Left(config.fpu),
                    acquisitionMirror = acqMirror,
                    camera            = config.camera,
                    focus             = config.focus,
                    readMode          = resolvedReadMode
                  )
          ss <- config.telescopeConfigs.telescopeConfigs.traverse(SeqState.scienceStep(_, ObserveClass.Science))
        yield ss

      StepDefinition(sciSteps)

  case class Generator(
    steps:      StepDefinition,
    builder:    AtomBuilder[GnirsDynamicConfig],
    goalCycles: NonNegInt
  ) extends SequenceGenerator[GnirsDynamicConfig]:

    override def generate: Stream[Pure, Atom[GnirsDynamicConfig]] =
      val scienceAtom: ProtoAtom[ProtoStep[GnirsDynamicConfig]] =
        ProtoAtom(ScienceCycleTitle.some, steps.scienceSteps)

      val atoms: List[ProtoAtom[ProtoStep[GnirsDynamicConfig]]] =
        List.fill(goalCycles.value)(scienceAtom)

      builder.buildStream(Stream.emits(atoms))

  private def definitionError(oid: Observation.Id, msg: String): OdbError =
    OdbError.SequenceUnavailable(oid, s"Could not generate a sequence for $oid: $msg".some)

  private def zeroExposureTime(oid: Observation.Id): OdbError =
    definitionError(oid, "GNIRS Long Slit requires a positive exposure time.")

  def instantiate(
    observationId: Observation.Id,
    estimator:     StepTimeEstimateCalculator[GnirsStaticConfig, GnirsDynamicConfig],
    static:        GnirsStaticConfig,
    namespace:     UUID,
    config:        Config,
    time:          Either[OdbError, IntegrationTime]
  ): Either[OdbError, SequenceGenerator[GnirsDynamicConfig]] =
    val posTime: Either[OdbError, IntegrationTime] =
      time.filterOrElse(
        _.exposureTime.toNonNegMicroseconds.value > 0,
        zeroExposureTime(observationId)
      )

    for
      t <- posTime
    yield
      val s = StepDefinition.compute(config, t)
      val c = s.cycleCount(t)
      Generator(
        s,
        AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Science),
        c
      ): SequenceGenerator[GnirsDynamicConfig]
