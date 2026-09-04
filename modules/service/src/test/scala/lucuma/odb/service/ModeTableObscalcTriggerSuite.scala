// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.IO
import lucuma.core.model.Observation
import lucuma.core.util.CalculationState
import lucuma.odb.graphql.TestUsers
import lucuma.odb.graphql.query.ExecutionTestSupport
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.all.*
import skunk.implicits.*

class ModeTableObscalcTriggerSuite extends ExecutionTestSupport:

  override val pi = TestUsers.Standard.pi(1, 30)
  override val validUsers = List(pi)

  private val registeredTablesMissingTrigger: IO[List[String]] =
    withSession: session =>
      session.execute(
        sql"""
          SELECT r.c_table_name
          FROM (SELECT DISTINCT c_table_name FROM t_observing_mode_registry) r
          WHERE NOT EXISTS (
            SELECT 1
            FROM information_schema.triggers t
            WHERE t.event_object_table = r.c_table_name
              AND t.action_statement LIKE '%obsid_obscalc_invalidate%'
          )
          ORDER BY 1
        """.query(text)
      )

  private def settleObscalcToReady(oid: Observation.Id): IO[Unit] =
    withSession: session =>
      session.execute(
        sql"""
          UPDATE t_obscalc
          SET c_obscalc_state = 'ready', c_last_update = now()
          WHERE c_observation_id = $observation_id
        """.command
      )(oid).void

  private def obscalcState(oid: Observation.Id): IO[CalculationState] =
    withSession: session =>
      session.unique(
        sql"SELECT c_obscalc_state FROM t_obscalc WHERE c_observation_id = $observation_id"
          .query(calculation_state)
      )(oid)

  // Direct SQL so nothing on t_observation can invalidate on the test's behalf.
  private def swapReadMode(oid: Observation.Id): IO[Unit] =
    withSession: session =>
      session.execute(
        sql"""
          UPDATE t_flamingos_2_mos t
          SET c_read_mode = (SELECT c_tag FROM t_f2_read_mode WHERE c_tag IS DISTINCT FROM t.c_read_mode LIMIT 1)
          WHERE c_observation_id = $observation_id
        """.command
      )(oid).void

  test("every registered observing mode table has the obscalc invalidate trigger"):
    registeredTablesMissingTrigger.map: missing =>
      assertEquals(missing, Nil, s"mode tables without obsid_obscalc_invalidate: ${missing.mkString(", ")}")

  test("editing a mode table row invalidates obscalc"):
    for
      pid    <- createProgramAs(pi)
      tid    <- createTargetWithProfileAs(pi, pid)
      oid    <- createFlamingos2MosObservationAs(pi, pid, List(tid))
      _      <- settleObscalcToReady(oid)
      before <- obscalcState(oid)
      _      <- swapReadMode(oid)
      after  <- obscalcState(oid)
    yield
      assertEquals(before, CalculationState.Ready)
      assertEquals(after, CalculationState.Pending)
