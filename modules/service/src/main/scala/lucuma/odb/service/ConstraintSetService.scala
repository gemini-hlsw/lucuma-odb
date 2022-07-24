// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.MonadCancelThrow
import cats.syntax.functor._
import lucuma.core.model.StandardRole.{Admin, Ngo, Pi, Staff}
import lucuma.core.model.{ConstraintSet, ElevationRange, GuestUser, Observation, Program, ServiceUser, StandardUser, User}
import lucuma.odb.data.Tag
import lucuma.odb.service.ProgramService.Statements.{existsAllocationForPartner, existsUserAsCoi, existsUserAsPi}
import lucuma.odb.util.Codecs._
import skunk._
import skunk.data.Completion
import skunk.implicits._

sealed trait ConstraintSetService[F[_]] {
  import ConstraintSetService.CreateConstraintSetResponse
  def createConstraintSet(
    pid: Program.Id,
    oid: Observation.Id,
    cs:  ConstraintSet
  ): F[CreateConstraintSetResponse]
}

object ConstraintSetService {

  sealed trait CreateConstraintSetResponse
  object CreateConstraintSetResponse {
    final case class Confusion(c: Completion) extends CreateConstraintSetResponse
    final case class NotAuthorized(user: User)                extends CreateConstraintSetResponse
    final case class ObservationNotFound(oid: Observation.Id) extends CreateConstraintSetResponse
    final case class Success(oid: Observation.Id)             extends CreateConstraintSetResponse
  }
  import CreateConstraintSetResponse._

  def fromSession[F[_]: MonadCancelThrow](u: User, s: Session[F]): ConstraintSetService[F] =
    new ConstraintSetService[F] {
      import Statements._

      override def createConstraintSet(
        pid: Program.Id,
        oid: Observation.Id,
        cs:  ConstraintSet
      ): F[CreateConstraintSetResponse] = {
        val a = insertConstraintSet(oid, cs) |+| void" " |+| whereFragment(pid, u)
        s.prepare(a.fragment.command).use { ps =>
          ps.execute(a.argument).map {
            case Completion.Insert(1) => Success(oid)
            case c                    => Confusion(c)
          }
        }
      }
    }

  object Statements {

    /*
      c_observation_id   d_observation_id primary key references t_observation(c_observation_id) on delete cascade,
  c_cloud_extinction d_tag            not null    references t_cloud_extinction(c_tag),
  c_image_quality    d_tag            not null    references t_image_quality(c_tag),
  c_sky_background   d_tag            not null    references t_sky_background(c_tag),
  c_water_vapor      d_tag            not null    references t_water_vapor(c_tag),
  c_air_mass_min     d_air_mass       null default 1.0,
  c_air_mass_max     d_air_mass       null default 2.0,
  c_hour_angle_min   d_hour_angle     null default null,
  c_hour_angle_max   d_hour_angle     null default null,
     */

    def insertConstraintSet(
      oid: Observation.Id,
      cs:  ConstraintSet
    ): AppliedFragment =
      sql"""
        insert into t_constraint_set (
          c_observation_id,
          c_cloud_extinction,
          c_image_quality,
          c_sky_background,
          c_water_vapor,
          c_air_mass_min,
          c_air_mass_max,
          c_hour_angle_min,
          c_hour_angle_max
        )
        select
          $observation_id,
          $cloud_extinction,
          $image_quality,
          $sky_background,
          $water_vapor,
          ${air_mass_range_value.opt},
          ${air_mass_range_value.opt},
          ${hour_angle_range_value.opt},
          ${hour_angle_range_value.opt}
      """.apply(
        oid ~
        cs.cloudExtinction ~
        cs.imageQuality ~
        cs.skyBackground ~
        cs.waterVapor ~
        ElevationRange.airMass.getOption(cs.elevationRange).map(_.min) ~
        ElevationRange.airMass.getOption(cs.elevationRange).map(_.max) ~
        ElevationRange.hourAngle.getOption(cs.elevationRange).map(_.minHours) ~
        ElevationRange.hourAngle.getOption(cs.elevationRange).map(_.maxHours)
      )

    // lifted from TargetService ... TODO: share?
    def whereFragment(pid: Program.Id, user: User): AppliedFragment =
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

  }
}
