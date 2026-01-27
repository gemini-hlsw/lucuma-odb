// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package util

import cats.data.NonEmptyList
import cats.data.State
import cats.syntax.option.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.SequenceType
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.odb.sequence.data.ProtoStep

import java.util.UUID

/**
 * AtomBuilder encapsulates the complicated task of creating Atom from a list
 * of proto-steps.  This requires calculating the atom and step ids from indices
 * and estimating the step cost.
 */
trait AtomBuilder[D]:

  def build(
    desc:  Option[NonEmptyString],
    aix:   Int,
    six:   Int,
    steps: NonEmptyList[ProtoStep[D]],
  ): State[TimeEstimateCalculator.Last[D], Atom[D]]

  def buildOption(
    desc:  Option[NonEmptyString],
    aix:   Int,
    six:   Int,
    steps: List[ProtoStep[D]]
  ): State[TimeEstimateCalculator.Last[D], Option[Atom[D]]] =
    NonEmptyList.fromList(steps) match
      case None      => State.pure(None)
      case Some(nel) => build(desc, aix, six, nel).map(_.some)

object AtomBuilder:

  def instantiate[S, D](
    estimator:    TimeEstimateCalculator[S, D],
    static:       S,
    namespace:    UUID,
    sequenceType: SequenceType
  ): AtomBuilder[D] =

    new AtomBuilder[D]:

      override def build(
        desc:      Option[NonEmptyString],
        aix:       Int,
        six:       Int,
        steps:     NonEmptyList[ProtoStep[D]]
      ): State[TimeEstimateCalculator.Last[D], Atom[D]] =
        State { (calcState: TimeEstimateCalculator.Last[D]) =>
          steps.mapAccumulate(calcState) { (e, s) =>
            val estimate = estimator.estimateStep(static, e, s)
            (e.next(s), estimate)
          }
        }.map: estimates =>
          val atomId = SequenceIds.atomId(namespace, sequenceType, aix)
          val nel    = steps.zip(estimates).zipWithIndex
          Atom(
            atomId,
            desc,
            nel.map { case ((protoStep, estimate), stepIndex) =>
              Step(
                SequenceIds.stepId(namespace, sequenceType, atomId, six + stepIndex),
                protoStep.value,
                protoStep.stepConfig,
                protoStep.telescopeConfig,
                estimate,
                protoStep.observeClass,
                protoStep.breakpoint
              )
            }
          )