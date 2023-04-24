// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.MonadCancelThrow
import cats.syntax.all._
import coulomb.rational.typeexpr.NonNegInt
import eu.timepit.refined.types.numeric.NonNegShort
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.Group
import lucuma.odb.graphql.input.CreateGroupInput
import lucuma.odb.graphql.input.GroupPropertiesInput
import lucuma.odb.util.Codecs._
import skunk._
import skunk.codec.all.*
import skunk.implicits._

trait GroupService[F[_]] {
  def createGroup(input: CreateGroupInput): F[Group.Id]
}

object GroupService {

  // TODO: check access control

  def fromSessionAndUser[F[_]: MonadCancelThrow](s: Session[F], user: User): GroupService[F] =
    new GroupService[F] {

      def createGroup(input: CreateGroupInput): F[Group.Id] =
        s.transaction.use { xa =>
          for {
            _ <- s.execute(sql"SET CONSTRAINTS ALL DEFERRED".command)
            i <- openHole(input.programId, input.SET.parentGroupId, input.SET.parentGroupIndex, xa)
            g <- s.prepareR(Statements.InsertGroup).use(_.unique(input ~ i))
          } yield g
        }

      def openHole(pid: Program.Id, gid: Option[Group.Id], index: Option[NonNegShort], xa: Transaction[F]): F[NonNegShort] =
        s.prepareR(Statements.OpenHole).use(_.unique(pid ~ gid ~ index))

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
         .contramap[CreateGroupInput ~ NonNegShort] { case c ~ index =>
          c.programId ~
          c.SET.parentGroupId ~
          index ~
          c.SET.name ~
          c.SET.description ~
          c.SET.minimumRequired ~
          c.SET.ordered ~
          c.SET.minimumInterval ~
          c.SET.maximumInterval
        }

    val OpenHole: Query[Program.Id ~ Option[Group.Id] ~ Option[NonNegShort], NonNegShort] =
      sql"select group_open_hole($program_id, ${group_id.opt}, ${int2_nonneg.opt})".query(int2_nonneg)

  }

}