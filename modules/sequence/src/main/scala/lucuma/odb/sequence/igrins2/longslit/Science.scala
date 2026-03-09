// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package igrins2.longslit

import cats.data.NonEmptyList
import cats.data.State
import cats.syntax.option.*
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepGuideState.Enabled
import lucuma.core.math.Offset
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.igrins2.{Igrins2DynamicConfig => IG2}

import lucuma.core.optics.syntax.lens.*
import lucuma.core.model.sequence.igrins2.Igrins2StaticConfig
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.util.AtomBuilder

import java.util.UUID

object Science extends SequenceState[IG2]:

  override def initialDynamicConfig: IG2 =
    IG2(60.secTimeSpan)

  def generator(
    estimator: TimeEstimateCalculator[Igrins2StaticConfig, IG2],
    static:    Igrins2StaticConfig,
    namespace: UUID,
    time:      IntegrationTime
  ): SequenceGenerator[IG2] =

    val builder = AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Science)

    def ig2ScienceStep(o: Offset): State[IG2, ProtoStep[IG2]] =
      scienceStep(TelescopeConfig(o, Enabled), ObserveClass.Science)

    val a0off = Offset(Offset.P.Zero, -LongSlit.NodQ)
    val b0off = Offset(Offset.P.Zero, LongSlit.NodQ)
    val b1off = Offset(Offset.P.Zero, LongSlit.NodQ)
    val a1off = Offset(Offset.P.Zero, -LongSlit.NodQ)

    val steps: NonEmptyList[ProtoStep[IG2]] =
      eval:
        for {
          _  <- IG2.exposure    := time.exposureTime
          s0 <- ig2ScienceStep(a0off)
          s1 <- ig2ScienceStep(b0off)
          s2 <- ig2ScienceStep(b1off)
          s3 <- ig2ScienceStep(a1off)
        } yield NonEmptyList.of(s0, s1, s2, s3)

    val protoAtom = ProtoAtom(LongSlit.ABBACycleTitle.some, steps)

    val cycleCount: Int =
      val exposuresPerCycle = 4
      val total = time.exposureCount.value
      (total + exposuresPerCycle - 1) / exposuresPerCycle

    new SequenceGenerator[IG2]:
      override def generate: Stream[Pure, Atom[IG2]] =
        Stream
          .range(0, cycleCount)
          .map(protoAtom -> _)
          .mapAccumulate(TimeEstimateCalculator.Last.empty[IG2]):
            case (calcState, (pa, aix)) =>
              val (csNew, atom) = builder.build(pa.description, aix, 0, pa.steps).run(calcState).value
              (csNew, atom)
          .map(_._2)
