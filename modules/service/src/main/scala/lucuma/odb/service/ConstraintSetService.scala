// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.MonadCancelThrow
import lucuma.core.model.StandardRole.{Admin, Ngo, Pi, Staff}
import lucuma.core.model.{GuestUser, Observation, Program, ServiceUser, StandardUser, User}
import lucuma.odb.data.Tag
import lucuma.odb.graphql.snippet.input.ConstraintSetInput
import lucuma.odb.service.ProgramService.Statements.{existsAllocationForPartner, existsUserAsCoi, existsUserAsPi}
import skunk._
import skunk.implicits._

sealed trait ConstraintSetService[F[_]] {
  import ConstraintSetService.CreateConstraintSetResponse
  def createConstraintSet(
    pid:   Program.Id,
    oid:   Observation.Id,
    input: ConstraintSetInput
  ): F[CreateConstraintSetResponse]
}

object ConstraintSetService {

  sealed trait CreateConstraintSetResponse
  object CreateConstraintSetResponse {
    final case class NotAuthorized(user: User)                extends CreateConstraintSetResponse
    final case class ObservationNotFound(oid: Observation.Id) extends CreateConstraintSetResponse
    final case class Success(oid: Observation.Id)             extends CreateConstraintSetResponse
  }
  import CreateConstraintSetResponse._

  def fromSession[F[_]: MonadCancelThrow](s: Session[F], u: User): ConstraintSetService[F] =
    new ConstraintSetService[F] {
      import Statements._

      override def createConstraintSet(
        pid:   Program.Id,
        oid:   Observation.Id,
        input: ConstraintSetInput
      ): F[CreateConstraintSetResponse] =
        ???
    }

  object Statements {

//    val InsertConstraintSet: Fragment[]
//        sql"""
//        """

    // lifted from TargetService ... TODO: share?
    def whereFragment(pid: Program.Id, user: User): AppliedFragment = {
      val insert =
        user match {
          case GuestUser(id)                => void"WHERE " |+| existsUserAsPi(pid, id)
          case ServiceUser(_, _)            => void""
          case StandardUser(id, role, _, _) =>
            role match {
              case Admin(_)        => void""
              case Ngo(_, partner) => void"WHERE " |+| existsAllocationForPartner(pid, Tag(partner.tag))
              case Pi(_)           => void"WHERE " |+| existsUserAsPi(pid, id) |+| void" OR " |+| existsUserAsCoi(pid, id)
              case Staff(_)        => void""
            }
        }
      insert |+| void" RETURNING c_target_id"
    }

  }
}
