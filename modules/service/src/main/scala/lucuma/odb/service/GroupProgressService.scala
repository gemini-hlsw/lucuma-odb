// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.all.*
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.group.GroupProgress
import lucuma.odb.data.GroupTree
import lucuma.odb.logic.GroupProgressEvaluator
import lucuma.odb.logic.GroupProgressEvaluator.ObservationFacts
import lucuma.odb.service.workflow.ObservationValidationInfo
import lucuma.odb.syntax.instrument.*
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.implicits.*

import Services.Syntax.*

/**
 * Evaluates the progress of groups toward satisfying their constraints.  The
 * rules live in `GroupProgressEvaluator`; this service loads the facts it
 * needs.
 */
trait GroupProgressService[F[_]]:

  /**
   * The progress of each group in `trees` (`GroupService.selectProgramTrees`),
   * as of the transaction time (the clock that also stamps execution events).
   * Deleted groups are absent from the result.
   */
  def selectProgress(
    trees: List[GroupService.ProgramTree]
  )(using Transaction[F]): F[Map[Group.Id, GroupProgress]]

object GroupProgressService:

  def instantiate[F[_]: Concurrent](using Services[F]): GroupProgressService[F] =
    new GroupProgressService[F]:

      override def selectProgress(
        trees: List[GroupService.ProgramTree]
      )(using Transaction[F]): F[Map[Group.Id, GroupProgress]] =
        // A group's progress depends only on its own subtree, so only the
        // subtrees under the requested groups are evaluated, and the
        // per-observation facts (the costly part) are loaded, in one query
        // across all the programs, only for the observations in them.
        val subtrees: List[(GroupService.ProgramTree, List[GroupTree.Branch])] =
          trees.fproduct(pt => pt.tree.topmostBranches(pt.groupIds.toSet))

        if subtrees.isEmpty then Map.empty[Group.Id, GroupProgress].pure[F]
        else
          for
            now   <- timeService.transactionTime
            facts <- selectFacts(subtrees.flatMap(_._2.flatMap(_.allObservations)))
          yield subtrees.flatMap: (pt, branches) =>
            val all = GroupProgressEvaluator.evaluate(branches, facts, now)
            pt.groupIds.flatMap(gid => all.get(gid).tupleLeft(gid))
          .toMap

      private def selectFacts(oids: List[Observation.Id]): F[Map[Observation.Id, ObservationFacts]] =
        if oids.isEmpty then Map.empty[Observation.Id, ObservationFacts].pure[F]
        else session.execute(Statements.SelectFacts)(oids).map(_.toMap)

  object Statements:

    private val facts: Decoder[(Observation.Id, ObservationFacts)] =
      (observation_id                       *:
       execution_state                      *:
       calibration_role.opt                 *:
       user_state.opt                       *:
       user_state.opt                       *:
       core_timestamp.opt                   *:
       core_timestamp.opt                   *:
       lucuma.odb.util.Codecs.site.opt      *:
       instrument.opt
      ).map: (oid, state, role, own, science, start, completion, visitSite, inst) =>
        // Inactive as the workflow decides it.
        val inactive = ObservationValidationInfo.effectiveUserState(role, own, science).contains(ObservationWorkflowState.Inactive)
        // Where it was observed, if it has been; otherwise its instrument's site.
        oid -> ObservationFacts(state, inactive, start, completion, visitSite.orElse(inst.map(_.site)))

    val SelectFacts: Query[List[Observation.Id], (Observation.Id, ObservationFacts)] =
      sql"""
        SELECT
          o.c_observation_id,
          t.c_execution_state,
          o.c_calibration_role,
          o.c_workflow_user_state,
          s.c_workflow_user_state,
          t.c_start_time,
          t.c_completion_time,
          (
            SELECT v.c_site
            FROM   t_visit v
            WHERE  v.c_observation_id = o.c_observation_id
            ORDER BY v.c_recorded_time DESC
            LIMIT 1
          ),
          o.c_instrument
        FROM t_observation o
        JOIN v_execution_timestamps t ON t.c_observation_id = o.c_observation_id
        ${ObservationValidationInfo.Statements.AssociatedScienceObservation}
        WHERE o.c_observation_id = ANY(${_gid[Observation.Id]})
          AND o.c_existence  = 'present'
      """.query(facts)
