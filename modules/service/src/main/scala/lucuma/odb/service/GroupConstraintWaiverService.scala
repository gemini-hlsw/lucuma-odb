// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import lucuma.core.enums.GroupConstraintViolationCode
import lucuma.core.enums.Site
import lucuma.core.model.Attribution
import lucuma.core.model.Group
import lucuma.core.model.LocalObservingNight
import lucuma.core.model.Observation
import lucuma.core.model.ObservingNight
import lucuma.core.model.User
import lucuma.core.model.group.GroupConstraintViolation
import lucuma.core.model.group.GroupConstraintViolation.Waivable
import lucuma.core.model.group.GroupConstraintWaiver
import lucuma.core.model.group.GroupProgress
import lucuma.core.syntax.string.*
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.input.RevokeGroupConstraintWaiverInput
import lucuma.odb.graphql.input.WaiveGroupConstraintInput
import lucuma.odb.syntax.groupConstraintViolation.*
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.boolean.bool
import skunk.codec.temporal.date
import skunk.implicits.*

import java.time.LocalDate

import Services.Syntax.*

/**
 * Waivers of violations of group constraints: loading the active ones for
 * evaluating progress, and granting and revoking them.
 */
trait GroupConstraintWaiverService[F[_]]:

  /** The active (unrevoked) waivers of the given groups, by group. */
  def selectActive(groupIds: List[Group.Id])(using Transaction[F]): F[Map[Group.Id, List[GroupConstraintWaiver]]]

  /**
   * Waives the violation in `input`, if `which` selects its group (it selects
   * the group if the user may edit it, and nothing otherwise).  The violation
   * must be one the group's progress currently lists, and so not already
   * waived.  A waiver of the same violation granted concurrently fails the
   * insert with a unique violation.
   */
  def waive(
    input: WaiveGroupConstraintInput,
    which: AppliedFragment
  )(using Transaction[F]): F[Result[GroupConstraintWaiver.Id]]

  /**
   * Revokes the waiver in `input`, if its group is among `which` (the groups
   * the user may edit) and it hasn't been revoked already.
   */
  def revoke(
    input: RevokeGroupConstraintWaiverInput,
    which: AppliedFragment
  )(using Transaction[F]): F[Result[GroupConstraintWaiver.Id]]

object GroupConstraintWaiverService:

  def instantiate[F[_]: Concurrent](using Services[F]): GroupConstraintWaiverService[F] =
    new GroupConstraintWaiverService[F]:

      override def selectActive(
        groupIds: List[Group.Id]
      )(using Transaction[F]): F[Map[Group.Id, List[GroupConstraintWaiver]]] =
        if groupIds.isEmpty then Map.empty[Group.Id, List[GroupConstraintWaiver]].pure[F]
        else session.execute(Statements.SelectActive)(groupIds.distinct).map(_.groupBy(_.groupId))

      override def waive(
        input: WaiveGroupConstraintInput,
        which: AppliedFragment
      )(using Transaction[F]): F[Result[GroupConstraintWaiver.Id]] =
        val gid = input.groupId
        val v   = input.violation

        def invalid(msg: String): Result[GroupConstraintWaiver.Id] =
          OdbError.InvalidArgument(msg.some).asFailure

        val af = Statements.selectsAny(which)
        session.prepareR(af.fragment.query(bool)).use(_.unique(af.argument)).flatMap:
          case false =>
            invalid(s"Group $gid doesn't exist, or you may not edit it.").pure[F]
          case true  =>
            for
              trees    <- groupService.selectProgramTrees(List(gid))
              progress <- groupProgressService.selectProgress(trees)
              result   <- progress.get(gid) match
                case None                                         =>
                  invalid(s"Group $gid has been deleted.").pure[F]
                case Some(p) if p.waived.exists(_.violation == v) =>
                  invalid(s"Group $gid's ${describe(v)} is already waived.").pure[F]
                case Some(p) if !violations(p).contains(v)        =>
                  invalid(s"Group $gid has no ${describe(v)} to waive.").pure[F]
                case Some(_)                                      =>
                  insert(gid, v, input)
            yield result

      private def insert(
        gid:   Group.Id,
        v:     Waivable,
        input: WaiveGroupConstraintInput
      ): F[Result[GroupConstraintWaiver.Id]] =
        val args = (
          gid,
          v.code,
          v.element.flatMap(_.left.toOption),
          v.element.flatMap(_.toOption),
          v.night.map(_.site),
          v.night.map(_.toLocalDate),
          user.id,
          input.reason
        )
        session.unique(Statements.Insert)(args).map(Result.success)

      override def revoke(
        input: RevokeGroupConstraintWaiverInput,
        which: AppliedFragment
      )(using Transaction[F]): F[Result[GroupConstraintWaiver.Id]] =
        val wid = input.waiverId
        val af  = Statements.revoke(wid, user.id, input.reason, which)
        session.prepareR(af.fragment.query(group_constraint_waiver_id)).use(_.option(af.argument)).flatMap:
          case Some(id) =>
            Result(id).pure[F]
          case None     =>
            val check = Statements.selectRevoked(wid, which)
            session.prepareR(check.fragment.query(bool)).use(_.option(check.argument)).map:
              case Some(true) =>
                OdbError.InvalidArgument(s"Waiver $wid has already been revoked.".some).asFailure
              case _          =>
                OdbError.InvalidArgument(s"Waiver $wid doesn't exist, or you may not edit its group.".some).asFailure

  // The violations the progress lists, none of them waived.
  private def violations(p: GroupProgress): List[GroupConstraintViolation] =
    p match
      case u: GroupProgress.Unschedulable => u.violations.toList
      case _                              => Nil

  private def describe(v: Waivable): String =
    val where =
      v.element.map(e => s"at ${e.merge}")
        .orElse(v.night.map(n => s"on the ${n.site.tag} night ending ${n.toLocalDate}"))
    (v.code.tag.toScreamingSnakeCase :: "violation" :: where.toList).mkString(" ")

  object Statements:

    // A waiver row.  The table's location check guarantees that each code
    // has the location it needs and that only waivable codes are stored.
    private val waiver: Decoder[GroupConstraintWaiver] =
      (group_constraint_waiver_id      *:
       group_id                        *:
       group_constraint_violation_code *:
       group_id.opt                    *:
       observation_id.opt              *:
       site.opt                        *:
       date.opt                        *:
       user_id                         *:
       core_timestamp                  *:
       text_nonempty.opt               *:
       user_id.opt                     *:
       core_timestamp.opt              *:
       text_nonempty.opt
      ).emap: (id, gid, code, elementGroup, elementObs, nightSite, nightDate, grantedBy, grantedAt, grantedReason, revokedBy, revokedAt, revokedReason) =>
        val element = elementGroup.map(_.asLeft[Observation.Id]).orElse(elementObs.map(_.asRight[Group.Id]))
        val night   = (nightSite, nightDate).mapN((s, d) => ObservingNight(s, LocalObservingNight(d)))
        Waivable.fromLocation(code, element, night).leftMap(m => s"Waiver $id: $m").map: v =>
          GroupConstraintWaiver(
            id,
            gid,
            v,
            Attribution(grantedBy, grantedAt, grantedReason),
            (revokedBy, revokedAt).mapN(Attribution(_, _, revokedReason))
          )

    val SelectActive: Query[List[Group.Id], GroupConstraintWaiver] =
      sql"""
        SELECT
          c_waiver_id,
          c_group_id,
          c_code,
          c_element_group_id,
          c_element_observation_id,
          c_night_site,
          c_night_date,
          c_granted_by,
          c_granted_at,
          c_granted_reason,
          c_revoked_by,
          c_revoked_at,
          c_revoked_reason
        FROM  t_group_constraint_waiver
        WHERE c_group_id = ANY(${_gid[Group.Id]})
          AND c_revoked_at IS NULL
      """.query(waiver)

    // Whether `which` selects anything.
    def selectsAny(which: AppliedFragment): AppliedFragment =
      void"SELECT EXISTS (" |+| which |+| void")"

    val Insert: Query[(Group.Id, GroupConstraintViolationCode, Option[Group.Id], Option[Observation.Id], Option[Site], Option[LocalDate], User.Id, Option[NonEmptyString]), GroupConstraintWaiver.Id] =
      sql"""
        INSERT INTO t_group_constraint_waiver (
          c_group_id,
          c_code,
          c_element_group_id,
          c_element_observation_id,
          c_night_site,
          c_night_date,
          c_granted_by,
          c_granted_reason
        ) VALUES (
          $group_id,
          $group_constraint_violation_code,
          ${group_id.opt},
          ${observation_id.opt},
          ${site.opt},
          ${date.opt},
          $user_id,
          ${text_nonempty.opt}
        )
        RETURNING c_waiver_id
      """.query(group_constraint_waiver_id)

    def revoke(
      wid:    GroupConstraintWaiver.Id,
      by:     User.Id,
      reason: Option[NonEmptyString],
      which:  AppliedFragment
    ): AppliedFragment =
      sql"""
        UPDATE t_group_constraint_waiver
           SET c_revoked_by     = $user_id,
               c_revoked_at     = now(),
               c_revoked_reason = ${text_nonempty.opt}
         WHERE c_waiver_id = $group_constraint_waiver_id
           AND c_revoked_at IS NULL
           AND c_group_id IN ("""((by, reason, wid)) |+| which |+| void""")
        RETURNING c_waiver_id
      """

    // Whether the waiver has been revoked, if its group is among `which`.
    def selectRevoked(wid: GroupConstraintWaiver.Id, which: AppliedFragment): AppliedFragment =
      sql"""
        SELECT c_revoked_at IS NOT NULL
        FROM   t_group_constraint_waiver
        WHERE  c_waiver_id = $group_constraint_waiver_id
          AND  c_group_id IN ("""(wid) |+| which |+| void""")
      """
