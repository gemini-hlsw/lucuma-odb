// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.effect.MonadCancelThrow
import cats.syntax.all._
import lucuma.core.model.Access.Admin
import lucuma.core.model.Access.Service
import lucuma.core.model.Access.Staff
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.Tag
import lucuma.odb.util.Codecs._
import skunk._
import skunk.codec.temporal.interval
import skunk.implicits._

import java.time.Duration
import lucuma.odb.graphql.snippet.input.SetAllocationInput
import lucuma.core.model.Partner

trait AllocationService[F[_]] {
  def setAllocation(input: SetAllocationInput): F[AllocationService.SetAllocationResponse]
}

object AllocationService {

  sealed trait SetAllocationResponse extends Product with Serializable
  object SetAllocationResponse {
    case class  NotAuthorized(user: User)        extends SetAllocationResponse
    case class  ProgramNotFound(pid: Program.Id) extends SetAllocationResponse
    case class  PartnerNotFound(partner: Tag)    extends SetAllocationResponse
    case object Success                          extends SetAllocationResponse
  }

  def fromSessionAndUser[F[_]: MonadCancelThrow](s: Session[F], user: User): AllocationService[F] =
    new AllocationService[F] {

      def setAllocation(input: SetAllocationInput): F[SetAllocationResponse] =
        user.role.access match {
          case Staff | Admin | Service =>
            s.prepare(Statements.SetAllocation.command).use { ps =>
              ps.execute(input.programId ~ input.partner ~ input.duration.value).as(SetAllocationResponse.Success)
            }
          case _ => Applicative[F].pure(SetAllocationResponse.NotAuthorized(user))
        }

    }

  object Statements {

    val SetAllocation: Fragment[Program.Id ~ Partner ~ Duration] =
        sql"""
          INSERT INTO t_allocation (c_program_id, c_partner, c_duration)
          VALUES ($program_id, $partner, $interval)
          ON CONFLICT (c_program_id, c_partner) DO UPDATE
          SET c_duration = $interval
        """.contramap { case p ~ t ~ d => p ~ t ~ d ~ d }

  }

}