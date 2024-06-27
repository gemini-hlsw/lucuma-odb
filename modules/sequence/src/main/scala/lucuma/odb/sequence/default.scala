// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.data.EitherT
import cats.data.State
import cats.syntax.functor.*
import fs2.Pipe
import lucuma.odb.sequence.data.Completion
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep

import Completion.SequenceMatch.matchAny

/**
 * Performs smart gcal expansion and simple filtering based on completion. The
 * atoms coming through the pipe are zipped with their original index, before
 * completion filtering.
 */
def defaultExpandAndFilter[F[_], D](
  expander: SmartGcalExpander[F, D],
  completion: Completion.SequenceMatch[D]
): Pipe[F, ProtoAtom[ProtoStep[D]], Either[String, (ProtoAtom[ProtoStep[D]], Long)]] =

  type MatchState[A] = State[Completion.SequenceMatch[D], A]

    // Do smart-gcal expansion
  _.through(expander.expandSequence)

    // Number the atoms, because executed atoms will be filtered out but
    // we need the correct index to always calculate the same Atom.Id.
    .zipWithIndex.map { case (e, index) => e.tupleRight(index) }

    // Mark the atoms with true (excuted) or false (not executed)
    .mapAccumulate(completion) { case (state, eAtom) =>
      val nextAtom = for {
        (atom, index) <- EitherT.fromEither[MatchState](eAtom)
        visit         <- EitherT.liftF(matchAny(atom))
      } yield (atom, index, visit.isDefined)

      nextAtom.value.run(state).value
    }

    // dump the state and keep only un-executed atoms
    .collect {
      case (_, Left(error))                 => Left(error)
      case (_, Right((atom, index, false))) => Right((atom, index))
    }
