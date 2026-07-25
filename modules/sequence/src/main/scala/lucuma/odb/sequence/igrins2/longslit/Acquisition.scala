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
 * IGRINS-2 SVC (Slit-Viewing Camera) acquisition. A single atom holding one
 * acquisition-class step per SVC telescope dither position, at the effective SVC exposure,
 * with a breakpoint on the final step so the observer can confirm centring before science
 * begins. There is no repeating atom: the SVC dither is a fixed pattern, not a centring
 * probe to iterate on. See docs/adr/igrins2-svc-acquisition-generation.md.
 */
object Acquisition:

  val AtomTitle: NonEmptyString = NonEmptyString.unsafeFrom("SVC Acquisition")

  private def steps(svc: Config.Svc): NonEmptyList[ProtoStep[Igrins2DynamicConfig]] =
    val protoSteps =
      Science.Igrins2SequenceState.eval:
        for
          _  <- State.modify[Igrins2DynamicConfig](_.copy(exposure = svc.exposure))
          ss <- svc.telescopeConfigs.traverse(Science.Igrins2SequenceState.igrins2ScienceStep(ObserveClass.Acquisition))
        yield ss
    NonEmptyList.fromListUnsafe(protoSteps.toList.init :+ protoSteps.last.withBreakpoint)

  private class Generator(
    builder: AtomBuilder[Igrins2DynamicConfig],
    svc:     Config.Svc
  ) extends SequenceGenerator[Igrins2DynamicConfig]:

    override val generate: Stream[Pure, Atom[Igrins2DynamicConfig]] =
      builder.buildStream(Stream.emit(ProtoAtom(AtomTitle.some, steps(svc))))

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
