// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package igrins2
package longslit

import cats.data.NonEmptyList
import cats.data.State
import cats.syntax.option.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2StaticConfig
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.util.AtomBuilder

import java.util.UUID

/**
 * IGRINS-2 SVC (Slit-Viewing Camera) acquisition.
 * An atom holding one acquisition-class step per SVC telescope offset position,
 * at the effective SVC exposure. The steps run through without pausing.
 *
 * The atom is followed by [[RepeatingAtomCount]] single-step atoms repeating the
 * last SVC offset, as GMOS and Flamingos-2 repeat their final acquisition step.
 *
 * IGRINS-2 acquisition is down with internal software used before we start the sequence.
 * Though we call this an acquisition the aim is to take some images before the science sequence
 * starts, and not to acquire the target in the slit.
 */
object Acquisition:

  val AtomTitle: NonEmptyString = NonEmptyString.unsafeFrom("SVC Acquisition")

  val RepeatAtomTitle: NonEmptyString = NonEmptyString.unsafeFrom("Additional SVC Images")

  /** Repetitions offered after the initial atom, matching the other instruments. */
  val RepeatingAtomCount: Int = 10

  private def steps(svc: Config.Svc): NonEmptyList[ProtoStep[Igrins2DynamicConfig]] =
    Science.Igrins2SequenceState.eval:
      for
        _  <- State.modify[Igrins2DynamicConfig](_.copy(exposure = svc.exposure))
        ss <- svc.telescopeConfigs.traverse(Science.Igrins2SequenceState.igrins2ScienceStep(ObserveClass.Acquisition))
      yield ss

  private class Generator(
    builder: AtomBuilder[Igrins2DynamicConfig],
    svc:     Config.Svc
  ) extends SequenceGenerator[Igrins2DynamicConfig]:

    override val generate: Stream[Pure, Atom[Igrins2DynamicConfig]] =
      val protoSteps    = steps(svc)
      val repeatingAtom = ProtoAtom(RepeatAtomTitle.some, NonEmptyList.one(protoSteps.last))
      val atoms         =
        ProtoAtom(AtomTitle.some, protoSteps) :: List.fill(RepeatingAtomCount)(repeatingAtom)
      builder.buildStream(Stream.emits(atoms))

  def instantiate(
    estimator: StepTimeEstimateCalculator[Igrins2StaticConfig, Igrins2DynamicConfig],
    static:    Igrins2StaticConfig,
    namespace: UUID,
    config:    Config
  ): Stream[Pure, Atom[Igrins2DynamicConfig]] =
    config.svc.fold(Stream.empty): svc =>
      new Generator(
        AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Acquisition),
        svc
      ).generate
