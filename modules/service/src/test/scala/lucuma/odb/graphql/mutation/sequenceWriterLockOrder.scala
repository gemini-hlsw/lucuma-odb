// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.syntax.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Step
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.boolean.bool
import skunk.codec.numeric.int4
import skunk.implicits.*

import scala.concurrent.duration.*

// Sequence writers (replace, reset acquisition, delete) must take the
// observation's execution lock before their first write to
// t_sequence_materialization, whose triggers lock the observation's t_obscalc
// row and delete its execution digest.  A step event takes the execution lock
// and then, when the step completes, the same t_obscalc and t_execution_digest
// rows.  Taking the execution lock any later inverts that order and the two
// deadlock.
//
// Each test holds the execution lock in one transaction, as a step event does,
// waits until the sequence writer is blocked on it, and then finishes the way
// a completing step's triggers do.  If the writer already holds the t_obscalc
// row, Postgres detects the deadlock and aborts one of the two.
class sequenceWriterLockOrder extends query.ExecutionTestSupportForGmos with ReplaceGmosNorthSequenceOps:

  private val SelectBackendPid: Query[Void, Int] =
    sql"SELECT pg_backend_pid()".query(int4)

  private val LockObservationExecution: Query[Observation.Id, Int] =
    sql"SELECT 1 FROM lock_observation_execution($observation_id)".query(int4)

  private val InvalidateObscalc: Command[Observation.Id] =
    sql"CALL invalidate_obscalc($observation_id)".command

  private val DeleteExecutionDigest: Command[Observation.Id] =
    sql"DELETE FROM t_execution_digest WHERE c_observation_id = $observation_id".command

  private val IsBlockedBy: Query[Int, Boolean] =
    sql"""
      SELECT EXISTS (
        SELECT 1 FROM pg_stat_activity WHERE $int4 = ANY(pg_blocking_pids(pid))
      )
    """.query(bool)

  private def awaitBlockedBy(watcher: Session[IO], pid: Int): IO[Unit] =
    watcher
      .unique(IsBlockedBy)(pid)
      .flatMap(b => (IO.sleep(50.millis) *> awaitBlockedBy(watcher, pid)).unlessA(b))

  /**
   * Runs `write` against a step event on the same observation that holds the
   * execution lock, then completes the step event and returns the result of
   * `write`.
   */
  private def writeDuringStepCompletion[A](oid: Observation.Id)(write: IO[A]): IO[A] =
    (session, session).tupled.use: (stepEvent, watcher) =>
      stepEvent.transaction.use: _ =>
        for
          pid <- stepEvent.unique(SelectBackendPid)
          _   <- stepEvent.unique(LockObservationExecution)(oid)
          f   <- write.start
          _   <- awaitBlockedBy(watcher, pid).timeout(30.seconds)
          _   <- stepEvent.execute(InvalidateObscalc)(oid)
          _   <- stepEvent.execute(DeleteExecutionDigest)(oid)
        yield f
      .flatMap(_.joinWithNever)

  private val setup: IO[Observation.Id] =
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
    yield o

  private def replaceScience(oid: Observation.Id): IO[List[Step.Id]] =
    val in = input(oid, SequenceType.Science, atomInput("Foo", stepInput(GmosNorthFilter.GPrime)))
    query(pi, mutation(Instrument.GmosNorth, in))
      .map(mutationOutput(Instrument.GmosNorth, _).flatMap(_._2))

  test("replace sequence during a step completion"):
    for
      o <- setup
      _ <- recordVisitAs(serviceUser, o)
      s <- writeDuringStepCompletion(o)(replaceScience(o))
      i <- scienceSequenceIds(pi, o)
    yield assertEquals(i.toList.flatMap(_._2), s)

  test("reset acquisition during a step completion"):
    for
      o <- setup
      _ <- recordVisitAs(serviceUser, o)
      _ <- writeDuringStepCompletion(o)(resetAcquisitionAs(serviceUser, o))
    yield ()

  test("delete sequence during a step completion"):
    for
      o <- setup
      _ <- replaceScience(o)
      _ <- writeDuringStepCompletion(o):
             query(pi, s"""
               mutation {
                 deleteSequence(input: { observationId: ${o.asJson} }) {
                   observation { id }
                 }
               }
             """)
      m <- withSession(_.unique(sql"SELECT EXISTS (SELECT 1 FROM t_sequence_materialization WHERE c_observation_id = $observation_id)".query(bool))(o))
    yield assert(!m, "the sequence should no longer be materialized")
