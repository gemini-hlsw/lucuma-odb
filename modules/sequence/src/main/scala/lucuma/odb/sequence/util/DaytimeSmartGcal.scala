// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package util

import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.functor.*
import cats.syntax.option.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.StepViolation
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep

import java.util.UUID

/**
 * A generated daytime calibration sequence: no acquisition, no ITC, one
 * titled atom per smart GCAL step, each expanded by SmartGcal.  A step
 * without a SmartGcal definition is left out and reported; a sequence left
 * with no step at all fails.
 */
object DaytimeSmartGcal:

  def sequence[F[_]: Monad, S, D](
    observationId: Observation.Id,
    estimator:     StepTimeEstimateCalculator[S, D],
    static:        S,
    namespace:     UUID,
    expander:      SmartGcalExpander[F, S, D],
    calibration:   String,
    atoms:         NonEmptyList[(NonEmptyString, ProtoStep[D])]
  ): F[Either[OdbError, SequenceGenerator[D]]] =
    atoms
      .traverse: (title, step) =>
        expander.expandOrReport(static, step, calibration).map: (missing, steps) =>
          (missing, NonEmptyList.fromList(steps).map(ProtoAtom(title.some, _)))
      .map: rs =>
        val missing    = rs.toList.flatMap(_._1).distinct
        val protoAtoms = rs.toList.flatMap(_._2)
        Either.cond(
          protoAtoms.nonEmpty,
          new SequenceGenerator[D]:
            private val builder = AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Science)
            override def violations: List[StepViolation] =
              missing
            def generate: Stream[Pure, Atom[D]] =
              builder.buildStream(Stream.emits(protoAtoms))
          ,
          definitionError(observationId, missing.headOption.fold(SmartGcalExpander.NoSteps)(_.message))
        )
