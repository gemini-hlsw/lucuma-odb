// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import cats.data.NonEmptyList
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Pure
import fs2.Stream
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.odb.data.OdbError

/**
 * A precursor to a lucuma.core.model.sequence.ExecutionConfig, which contains
 * a concrete list of steps.  The StreamingExecutionConfig provides a `Stream`
 * of steps of a generic type because there are multiple uses for a sequence.
 * For example to calculate the sequence digest we never need to hold the whole
 * sequence in memory but instead can fold over the stream.
 *
 * @tparam S static execution config
 * @tparam A step type
 */
case class StreamingExecutionConfig[F[_], S, D](
  static:      S,
  acquisition: Stream[F, Atom[D]],
  science:     Stream[F, Atom[D]]
)

object StreamingExecutionConfig:

  extension [S, D](self: StreamingExecutionConfig[Pure, S, D])
    def covary[F[_]]: StreamingExecutionConfig[F, S, D] =
      StreamingExecutionConfig[F, S, D](
        self.static,
        self.acquisition.covary[F],
        self.science.covary[F]
      )

    /**
     * Converts the science sequence of a StreamingExecutionConfig into a
     * one-atom sequence containing all the steps of whatever atoms it otherwise
     * would contain.
     *
     * @param observationId observation id (used for the message if there is an error)
     * @param description atom description
     * @param stepLimit maximum number of steps that may appear in the atom
     *
     * @return a new StreamingExecutionConfig with a single-atom science sequence
     */
    def unsplit(
      observationId: Observation.Id,
      description: NonEmptyString = UnsplittableAtom.Description,
      stepLimit: PosInt           = UnsplittableAtom.StepLimit
    ): Either[OdbError, StreamingExecutionConfig[Pure, S, D]]  =
      self.science.take(1).toList match
        case Nil       =>
          self.asRight
        case head :: _ =>
          val allSteps =
            self.science
              .flatMap(a => Stream.emits(a.steps.toList))
              .take(stepLimit.value.toLong + 1)
              .toList
          Either.cond(
            allSteps.sizeIs <= stepLimit.value,
            self.copy(science = Stream.emit(Atom(head.id, description.some, NonEmptyList.fromListUnsafe(allSteps)))),
            OdbError.SequenceUnavailable(observationId, s"Unsplittable observation contains too many (> ${stepLimit.value}) steps.".some)
          )