// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.MonadCancelThrow
import cats.syntax.all._
import grackle.Result
import lucuma.core.model.Access.Admin
import lucuma.core.model.Access.Service
import lucuma.core.model.Access.Staff
import lucuma.core.model.Program
import lucuma.core.util.TimeSpan
import lucuma.odb.OdbError
import lucuma.odb.OdbErrorExtensions.*
import lucuma.odb.data.Tag
import lucuma.odb.graphql.input.SetAllocationInput
import lucuma.odb.util.Codecs._
import skunk._
import skunk.implicits._

import Services.Syntax.*

trait AllocationService[F[_]] {
  def setAllocation(input: SetAllocationInput)(using Transaction[F]): F[Result[Unit]]
}

object AllocationService {

  def instantiate[F[_]: MonadCancelThrow](using Services[F]): AllocationService[F] =
    new AllocationService[F] {
      def setAllocation(input: SetAllocationInput)(using Transaction[F]): F[Result[Unit]] =
        user.role.access match {
          case Staff | Admin | Service =>
            session.prepareR(Statements.SetAllocation.command).use { ps =>
              ps.execute(input.programId, input.partner, input.duration).as(Result.success(()))
            }
          case _ => OdbError.NotAuthorized(user.id).asFailureF
        }
    }

  object Statements {

    val SetAllocation: Fragment[(Program.Id, Tag, TimeSpan)] =
        sql"""
          INSERT INTO t_allocation (c_program_id, c_partner, c_duration)
          VALUES ($program_id, $tag, $time_span)
          ON CONFLICT (c_program_id, c_partner) DO UPDATE
          SET c_duration = $time_span
        """.contramap { case (p, t, d) => (p, t, d, d) }

  }

}
