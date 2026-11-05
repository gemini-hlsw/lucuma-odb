// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package util

import cats.Monad
import cats.data.EitherT
import cats.data.NonEmptyList
import cats.syntax.option.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep

import java.util.UUID

/**
 * A generated daytime calibration sequence: no acquisition, no ITC, one
 * titled atom per smart GCAL step, each expanded by SmartGcal.  A step
 * without a SmartGcal row fails the whole sequence.
 */
object DaytimeSmartGcal:

  def sequence[F[_]: Monad, S, D](
    observationId: Observation.Id,
    estimator:     StepTimeEstimateCalculator[S, D],
    static:        S,
    namespace:     UUID,
    expander:      SmartGcalExpander[F, S, D],
    atoms:         NonEmptyList[(NonEmptyString, ProtoStep[D])]
  ): F[Either[OdbError, SequenceGenerator[D]]] =
    atoms
      .traverse: (title, step) =>
        EitherT(expander.expandStep(static, step)).map(steps => ProtoAtom(title.some, steps))
      .bimap(
        m => definitionError(observationId, m),
        atoms =>
          val builder = AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Science)
          new SequenceGenerator[D]:
            def generate: Stream[Pure, Atom[D]] =
              builder.buildStream(Stream.emits(atoms.toList))
      ).value
