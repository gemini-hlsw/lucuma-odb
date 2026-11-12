// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package util

import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.bifunctor.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.SmartGcalExpander.Calibration
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
    calibration:   Calibration,
    atoms:         NonEmptyList[(NonEmptyString, ProtoStep[D])]
  ): F[Either[OdbError, SequenceGenerator[D]]] =
    atoms.toList
      .foldMapM: (title, step) =>
        expander.expandAtom(static, ProtoAtom(title.some, NonEmptyList.one(step)), calibration).map(_.map(_.toList))
      .map: (missing, protoAtoms) =>
        SmartGcalExpander.remaining(missing, protoAtoms).bimap(
          definitionError(observationId, _),
          (missing, protoAtoms) =>
            val builder = AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Science)
            val generator = new SequenceGenerator[D]:
              def generate: Stream[Pure, Atom[D]] =
                builder.buildStream(Stream.emits(protoAtoms))
            generator.reporting(missing)
        )
