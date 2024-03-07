// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all._
import eu.timepit.refined.types.numeric.NonNegShort
import grackle.Result
import lucuma.core.model.Group
import lucuma.core.model.Program
import lucuma.odb.data.GroupTree
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.input.CreateGroupInput
import lucuma.odb.graphql.input.GroupPropertiesInput
import lucuma.odb.util.Codecs._
import skunk._
import skunk.codec.all.*
import skunk.implicits._

import Services.Syntax.*

trait GroupService[F[_]] {
  def createGroup(input: CreateGroupInput)(using Transaction[F]): F[Result[Group.Id]]
  def updateGroups(SET: GroupPropertiesInput.Edit, which: AppliedFragment)(using Transaction[F]): F[Result[List[Group.Id]]]
  def selectGroups(programId: Program.Id): F[GroupTree]
  def selectPid(groupId: Group.Id): F[Option[Program.Id]]
}

object GroupService {

  // TODO: check access control

  def instantiate[F[_]: Concurrent](using Services[F]): GroupService[F] =
    new GroupService[F] {

      private def createGroupImpl(pid: Program.Id, SET: GroupPropertiesInput.Create)(using Transaction[F]): F[Group.Id] =
        for {
          _ <- session.execute(sql"SET CONSTRAINTS ALL DEFERRED".command)
          i <- openHole(pid, SET.parentGroupId, SET.parentGroupIndex)
          g <- session.prepareR(Statements.InsertGroup).use(_.unique((pid, SET), i))
        } yield g

      override def createGroup(input: CreateGroupInput)(using Transaction[F]): F[Result[Group.Id]] =
        programService.resolvePid(input.programId, input.proposalReference, input.programReference).flatMap: r =>
          r.traverse(createGroupImpl(_, input.SET))

      // Applying the same move to a list of groups will put them all together in the
      // destination group (or at the top level) in no particular order. Returns the ids of
      // directly affected groups, but not those who were shuffled as a side-effect.
      def moveGroups(
        groupId: Nullable[Group.Id],
        groupIndex: Option[NonNegShort],
        which: AppliedFragment
      ): F[List[Group.Id]] =
        (groupId, groupIndex) match
          case (Nullable.Absent, None) => Nil.pure[F] // do nothing if neither is specified
          case (gid, index) =>
            val af = Statements.moveGroups(gid.toOption, index, which)
            session.prepareR(af.fragment.query(group_id *: void)).use(pq => pq.stream(af.argument, 512).map(_._1).compile.toList)

      def updateGroups(SET: GroupPropertiesInput.Edit, which: AppliedFragment)(using Transaction[F]): F[Result[List[Group.Id]]] =
        session.execute(sql"SET CONSTRAINTS ALL DEFERRED".command) >>
        moveGroups(SET.parentGroupId, SET.parentGroupIndex, which).flatMap { ids =>
          Statements.updateGroups(SET, which).traverse { af =>
            session.prepareR(af.fragment.query(group_id)).use { pq => pq.stream(af.argument, 512).compile.toList }
          } .map(moreIds => Result(moreIds.foldLeft(ids)((a, b) => (a ++ b).distinct)))
        }

      def openHole(pid: Program.Id, gid: Option[Group.Id], index: Option[NonNegShort])(using Transaction[F]): F[NonNegShort] =
        session.prepareR(Statements.OpenHole).use(_.unique(pid, gid, index))

      def selectGroups(programId: Program.Id): F[GroupTree] = {

        def mkTree(m: Map[Option[Group.Id], List[GroupTree.Child]]): GroupTree = {

          def mapChildren(children: List[GroupTree.Child]): List[GroupTree.Child] =
            children.map {
              case l@GroupTree.Leaf(_)                        => l
              case b@GroupTree.Branch(_, _, _, _, _, _, _, _) => mapBranch(b)
            }

          def mapBranch(p: GroupTree.Branch): GroupTree.Branch =
            p.copy(children = mapChildren(m.get(p.groupId.some).toList.flatten))

          GroupTree.Root(programId, mapChildren(m.get(none[Group.Id]).toList.flatten))
        }

        for {
          gs <- session.execute(Statements.SelectGroups)(programId)
          os <- session.execute(Statements.SelectObservations)(programId)
        } yield mkTree((gs ++ os).groupBy(_._1).view.mapValues(_.sortBy(_._2.value).map(_._3)).toMap)

      }

      def selectPid(groupId: Group.Id): F[Option[Program.Id]] =
        session.option(Statements.SelectPid)(groupId)

    }

  object Statements {

    val InsertGroup: Query[Program.Id ~ GroupPropertiesInput.Create ~ NonNegShort, Group.Id] =
      sql"""
      insert into t_group (
        c_program_id,
        c_parent_id,
        c_parent_index,
        c_name,
        c_description,
        c_min_required,
        c_ordered,
        c_min_interval,
        c_max_interval
      ) values (
        $program_id,
        ${group_id.opt},
        ${int2_nonneg},
        ${text_nonempty.opt},
        ${text_nonempty.opt},
        ${int2_nonneg.opt},
        $bool,
        ${time_span.opt},
        ${time_span.opt}
      ) returning c_group_id
      """.query(group_id)
         .contramap[Program.Id ~ GroupPropertiesInput.Create ~ NonNegShort] { case ((pid, c), index) => (
          pid,
          c.parentGroupId,
          index,
          c.name,
          c.description,
          c.minimumRequired,
          c.ordered,
          c.minimumInterval,
          c.maximumInterval
        )}

    val OpenHole: Query[(Program.Id, Option[Group.Id], Option[NonNegShort]), NonNegShort] =
      sql"select group_open_hole($program_id, ${group_id.opt}, ${int2_nonneg.opt})".query(int2_nonneg)

    def updateGroups(SET: GroupPropertiesInput.Edit, which: AppliedFragment): Option[AppliedFragment] = {

      val fieldUpdates: List[AppliedFragment] =
        List(
          SET.name.toOptionOption.map(sql"c_name = ${text_nonempty.opt}"),
          SET.description.toOptionOption.map(sql"c_description = ${text_nonempty.opt}"),
          SET.ordered.map(sql"c_ordered = $bool"),
          SET.minimumRequired.toOptionOption.map(sql"c_min_required = ${int2_nonneg.opt}"),
          SET.minimumInterval.toOptionOption.map(sql"c_min_interval = ${time_span.opt}"),
          SET.maximumInterval.toOptionOption.map(sql"c_max_interval = ${time_span.opt}"),
        ).flatten

      NonEmptyList.fromList(fieldUpdates).map { updates =>
        updates.foldSmash(
          void"UPDATE t_group SET ",
          void", ",
          void" WHERE c_group_id IN (" |+| which |+| void") RETURNING c_group_id"
        )
      }

    }

    def moveGroups(gid: Option[Group.Id], index: Option[NonNegShort], which: AppliedFragment): AppliedFragment =
      sql"""
        SELECT c_group_id, group_move_group(c_group_id, ${group_id.opt}, ${int2_nonneg.opt})
        FROM t_group
        WHERE c_group_id IN (
      """.apply(gid, index) |+| which |+| void")"

    val branch: Decoder[GroupTree.Branch] =
      (group_id *: text_nonempty.opt *: text_nonempty.opt *: int2_nonneg.opt *: bool *:  time_span.opt *: time_span.opt).map {
        case (gid, name, description, minRequired, ordered, minInterval, maxInterval) =>
          GroupTree.Branch(gid, minRequired, ordered, Nil, name, description, minInterval, maxInterval)
      }

    val SelectGroups: Query[Program.Id, (Option[Group.Id], NonNegShort, GroupTree.Branch)] =
      sql"""
        SELECT
          c_parent_id,
          c_parent_index,
          c_group_id,
          c_name,
          c_description,
          c_min_required,
          c_ordered,
          c_min_interval,
          c_max_interval
        FROM
          t_group
        WHERE
          c_program_id = $program_id
      """.query(group_id.opt *: int2_nonneg *: branch)

    val SelectObservations: Query[Program.Id, (Option[Group.Id], NonNegShort, GroupTree.Leaf)] =
      sql"""
        SELECT
          c_group_id,
          c_group_index,
          c_observation_id
        FROM
          t_observation
        WHERE
          c_program_id = $program_id AND c_existence = 'present'
      """.query(group_id.opt *: int2_nonneg *: observation_id)
         .map { case (gid, index, oid) => (gid, index, GroupTree.Leaf(oid)) }

     val SelectPid: Query[Group.Id, Program.Id] =
       sql"""
         SELECT
           c_program_id
         FROM
           t_group
         WHERE
           c_group_id = $group_id
       """.query(program_id)
  }

}
