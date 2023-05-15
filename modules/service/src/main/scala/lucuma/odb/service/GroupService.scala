// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.effect.kernel.Sync
import cats.syntax.all._
import coulomb.rational.typeexpr.NonNegInt
import eu.timepit.refined.types.numeric.NonNegShort
import lucuma.core.model.Group
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.input.CreateGroupInput
import lucuma.odb.graphql.input.GroupPropertiesInput
import lucuma.odb.util.Codecs._
import skunk._
import skunk.codec.all.*
import skunk.implicits._

trait GroupService[F[_]] {
  def createGroup(input: CreateGroupInput): F[Group.Id]
  def updateGroups(SET: GroupPropertiesInput.Edit, which: AppliedFragment): F[GroupService.UpdateGroupsResponse]
}

object GroupService {

  enum UpdateGroupsResponse:
    case Success(selected: List[Group.Id])
    case Error(message: String)

  // TODO: check access control

  def fromSessionAndUser[F[_]: Sync](s: Session[F], user: User): GroupService[F] =
    new GroupService[F] {

      def createGroup(input: CreateGroupInput): F[Group.Id] =
        s.transaction.use { xa =>
          for {
            _ <- s.execute(sql"SET CONSTRAINTS ALL DEFERRED".command)
            i <- openHole(input.programId, input.SET.parentGroupId, input.SET.parentGroupIndex, xa)
            g <- s.prepareR(Statements.InsertGroup).use(_.unique(input, i))
          } yield g
        }

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
            s.prepareR(af.fragment.query(group_id *: void)).use(pq => pq.stream(af.argument, 512).map(_._1).compile.toList)

      def updateGroups(SET: GroupPropertiesInput.Edit, which: AppliedFragment): F[UpdateGroupsResponse] =
        s.transaction.use { xa =>
          s.execute(sql"SET CONSTRAINTS ALL DEFERRED".command) >>
          moveGroups(SET.parentGroupId, SET.parentGroupIndex, which).flatMap { ids =>
            Statements.updateGroups(SET, which).traverse { af =>
              s.prepareR(af.fragment.query(group_id)).use { pq => pq.stream(af.argument, 512).compile.toList }
            } .map(moreIds => UpdateGroupsResponse.Success(moreIds.foldLeft(ids)((a, b) => (a ++ b).distinct)))
          }
        }

      def openHole(pid: Program.Id, gid: Option[Group.Id], index: Option[NonNegShort], xa: Transaction[F]): F[NonNegShort] =
        s.prepareR(Statements.OpenHole).use(_.unique(pid, gid, index))

    }

  object Statements {

    val InsertGroup: Query[CreateGroupInput ~ NonNegShort, Group.Id] =
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
         .contramap[CreateGroupInput ~ NonNegShort] { case (c, index) => (
          c.programId,
          c.SET.parentGroupId,
          index,
          c.SET.name,
          c.SET.description,
          c.SET.minimumRequired,
          c.SET.ordered,
          c.SET.minimumInterval,
          c.SET.maximumInterval
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

  }

}
