// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.Partner
import lucuma.core.model.PartnerLeads
import lucuma.core.model.PartnerLeads.Candidate
import lucuma.core.model.Program
import lucuma.core.model.ProgramUser
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.boolean.bool
import skunk.syntax.all.*

import Services.Syntax.*

/**
 * Keeps each program's partner leads in line with its partner splits and its team, by the rules in
 * lucuma-core's `PartnerLeads`.  Every service that changes the splits or the team calls `sync` when
 * it is done, so that leads follow automatically whichever way the change arrives.
 */
trait PartnerLeadService[F[_]]:

  /**
   * Brings the partner leads of program `pid` in line with its splits and team.  `choices` carries
   * explicit choices for some partners: `Some(id)` makes `id` the lead, if eligible, and `None`
   * discards the current lead in favor of the default.  Partners without a choice keep their
   * current lead while it remains eligible.
   */
  def sync(
    pid:     Program.Id,
    choices: Map[Partner, Option[ProgramUser.Id]]
  )(using Transaction[F]): F[Result[Unit]]

  /** Brings the partner leads of each program in line with its splits and team. */
  def syncAll(pids: List[Program.Id])(using Transaction[F]): F[Unit]

  /**
   * The partners with requested time that lack a lead even though some investigator is eligible to
   * lead them.
   */
  def missing(pid: Program.Id)(using Transaction[F]): F[List[Partner]]

object PartnerLeadService:

  def instantiate[F[_]: Concurrent](using Services[F]): PartnerLeadService[F] =
    new PartnerLeadService[F]:

      override def sync(
        pid:     Program.Id,
        choices: Map[Partner, Option[ProgramUser.Id]]
      )(using Transaction[F]): F[Result[Unit]] =
        for
          requested <- session.execute(Statements.SelectRequestedPartners)(pid)
          rows      <- session.execute(Statements.SelectTeam)(pid)
          team       = rows.map((c, _) => c)
          current    = currentLeads(rows)
          // A choice for a partner without requested time has nothing to apply to.
          relevant   = choices.filter((p, _) => requested.contains(p))
          valid      = relevant.toList.traverse_ {
                         case (p, Some(id)) =>
                           PartnerLeads.validateSelection(p, id, team).fold(m => OdbError.InvalidArgument(m.some).asFailure, _ => Result.unit)
                         case _             =>
                           Result.unit
                       }
          res       <- valid.traverse: _ =>
                         val chosen = relevant.foldLeft(current):
                           case (m, (p, Some(id))) => m.updated(p, id)
                           case (m, (p, None))     => m - p
                         write(PartnerLeads.normalize(requested.toSet, team, chosen).values.toSet, current.values.toSet)
        yield res

      override def syncAll(pids: List[Program.Id])(using Transaction[F]): F[Unit] =
        pids.distinct.traverse_(sync(_, Map.empty))

      override def missing(pid: Program.Id)(using Transaction[F]): F[List[Partner]] =
        (session.execute(Statements.SelectRequestedPartners)(pid), session.execute(Statements.SelectTeam)(pid))
          .mapN: (requested, rows) =>
            PartnerLeads.missing(requested.toSet, rows.map(_._1), currentLeads(rows))

      private def currentLeads(rows: List[(Candidate, Boolean)]): Map[Partner, ProgramUser.Id] =
        rows.collect { case (c, true) => c }.flatMap(c => c.leadPartner.tupleRight(c.id)).toMap

      // Clear first, then set, so that no partner ever has two leads at once.
      private def write(
        leads:  Set[ProgramUser.Id],
        before: Set[ProgramUser.Id]
      ): F[Unit] =
        val clear = NonEmptyList.fromList((before -- leads).toList)
        val set   = NonEmptyList.fromList((leads -- before).toList)
        clear.traverse_(ids => session.execute(Statements.setLead(ids))((false, ids.toList))) *>
          set.traverse_(ids => session.execute(Statements.setLead(ids))((true, ids.toList)))

  object Statements:

    val SelectRequestedPartners: Query[Program.Id, Partner] =
      sql"""
        SELECT s.c_partner
          FROM t_partner_split s
          JOIN t_proposal p ON p.c_program_id = s.c_program_id
         WHERE s.c_program_id = $program_id
           AND s.c_percent > 0
      """.query(partner)

    val SelectTeam: Query[Program.Id, (Candidate, Boolean)] =
      sql"""
        SELECT c_program_user_id,
               c_role,
               c_partner_link,
               c_gemini_partner,
               c_exchange_partner,
               c_partner_lead
          FROM t_program_user
         WHERE c_program_id = $program_id
           AND c_role IN ('pi', 'coi', 'coi_ro')
         ORDER BY c_program_user_id
      """.query(program_user_id *: program_user_role *: partner_link *: bool).map:
        case (id, role, link, lead) => (Candidate(id, role, link), lead)

    def setLead(ids: NonEmptyList[ProgramUser.Id]): Command[(Boolean, List[ProgramUser.Id])] =
      sql"""
        UPDATE t_program_user
           SET c_partner_lead = $bool
         WHERE c_program_user_id IN (${program_user_id.list(ids.size)})
      """.command
