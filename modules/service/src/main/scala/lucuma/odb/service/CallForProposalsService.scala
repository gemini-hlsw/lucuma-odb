// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.effect.MonadCancelThrow
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import grackle.Result
import grackle.ResultT
import grackle.syntax.*
import lucuma.core.enums.Instrument
import lucuma.core.model.CallForProposals
import lucuma.core.model.Semester
import lucuma.core.util.TimestampInterval
import lucuma.odb.data.CallForProposalsType
import lucuma.odb.data.Nullable
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.input.CallForProposalsPartnerInput
import lucuma.odb.graphql.input.CallForProposalsPropertiesInput
import lucuma.odb.graphql.input.CreateCallForProposalsInput
import lucuma.odb.util.Codecs.*
import skunk.AppliedFragment
import skunk.Command
import skunk.Query
import skunk.SqlState
import skunk.Transaction
import skunk.implicits.*

import Services.Syntax.*

trait CallForProposalsService[F[_]] {

  def typeAndSemesterOf(
    cid: CallForProposals.Id
  )(using Transaction[F]): F[Option[(CallForProposalsType, Semester)]]

  def createCallForProposals(
    input: CreateCallForProposalsInput
  )(using Transaction[F], Services.StaffAccess): F[Result[CallForProposals.Id]]

  def updateCallsForProposals(
    SET:   CallForProposalsPropertiesInput.Edit,
    which: AppliedFragment
  )(using Transaction[F], Services.StaffAccess): F[Result[List[CallForProposals.Id]]]

}

object CallForProposalsService {

  def instantiate[F[_]: MonadCancelThrow: Concurrent](using Services[F]): CallForProposalsService[F] =
    new CallForProposalsService[F] {

      override def typeAndSemesterOf(
        cid: CallForProposals.Id
      )(using Transaction[F]): F[Option[(CallForProposalsType, Semester)]] =
        session.option(Statements.SelectTypeAndSemester)(cid)

      override def createCallForProposals(
        input: CreateCallForProposalsInput
      )(using Transaction[F], Services.StaffAccess): F[Result[CallForProposals.Id]] = {
        val partners    = input.SET.partners
        val instruments = input.SET.instruments
        (for {
          cid <- session.unique(Statements.InsertCallForProposals)(input.SET)
          cids = List(cid)
          _   <- session
                   .prepareR(Statements.InsertPartners(cids, partners))
                   .use(_.execute(cids, partners))
                   .whenA(partners.nonEmpty)
          _   <- session
                   .prepareR(Statements.InsertInstruments(cids, instruments))
                   .use(_.execute(cids, instruments))
                   .whenA(instruments.nonEmpty)
        } yield cid).map(_.success)
      }

      private def updateCfpTable(
        SET:   CallForProposalsPropertiesInput.Edit,
        which: AppliedFragment
      ): F[Result[List[CallForProposals.Id]]] = {
        val af = Statements.UpdateCallsForProposals(SET, which)
        session.prepareR(af.fragment.query(cfp_id)).use { pq =>
          pq.stream(af.argument, chunkSize = 1024)
            .compile
            .toList
            .map(_.success)
            .recover { case SqlState.DataException(_) =>
              OdbError.InvalidArgument("Requested update to the active period is invalid: activeStart must come before activeEnd".some).asFailure
            }
        }
      }

      private def updatePartners(
        cids:     List[CallForProposals.Id],
        partners: Nullable[List[CallForProposalsPartnerInput]]
      ): F[Result[Unit]] = {
        val delete =
          session.executeCommand(Statements.DeletePartners(cids)).void

        def insert(vals: List[CallForProposalsPartnerInput]) =
          session
            .prepareR(Statements.InsertPartners(cids, vals))
            .use(_.execute(cids, vals))
            .whenA(vals.nonEmpty)
            .void

        partners
          .fold(delete, Concurrent[F].unit, is => delete *> insert(is))
          .map(_.success)
      }

      private def updateInstruments(
        cids:        List[CallForProposals.Id],
        instruments: Nullable[List[Instrument]]
      ): F[Result[Unit]] = {
        val delete =
          session.executeCommand(Statements.DeleteInstruments(cids)).void

        def insert(vals: List[Instrument]) =
          session
            .prepareR(Statements.InsertInstruments(cids, vals))
            .use(_.execute(cids, vals))
            .whenA(vals.nonEmpty)
            .void

        instruments
          .fold(delete, Concurrent[F].unit, is => delete *> insert(is))
          .map(_.success)
      }

      override def updateCallsForProposals(
        SET:   CallForProposalsPropertiesInput.Edit,
        which: AppliedFragment
      )(using Transaction[F], Services.StaffAccess): F[Result[List[CallForProposals.Id]]] =
        (for {
          cids <- ResultT(updateCfpTable(SET, which))
          _    <- ResultT(updatePartners(cids, SET.partners))
          _    <- ResultT(updateInstruments(cids, SET.instruments))
        } yield cids).value

    }

  object Statements {

    val SelectTypeAndSemester: Query[CallForProposals.Id, (CallForProposalsType, Semester)] =
      sql"""
        SELECT c_type, c_semester
          FROM t_cfp
         WHERE c_cfp_id = $cfp_id AND c_existence = 'present'::e_existence
      """.query(cfp_type *: semester)

    val InsertCallForProposals: Query[CallForProposalsPropertiesInput.Create, CallForProposals.Id] =
      sql"""
        INSERT INTO t_cfp (
          c_type,
          c_semester,
          c_ra_start,
          c_ra_end,
          c_dec_start,
          c_dec_end,
          c_active,
          c_existence
        )
        SELECT
          $cfp_type,
          $semester,
          ${right_ascension.opt},
          ${right_ascension.opt},
          ${declination.opt},
          ${declination.opt},
          $timestamp_interval_tsrange,
          $existence
        RETURNING
          c_cfp_id
      """.query(cfp_id).contramap { input => (
        input.cfpType,
        input.semester,
        input.raLimit.map(_._1),
        input.raLimit.map(_._2),
        input.decLimit.map(_._1),
        input.decLimit.map(_._2),
        input.active,
        input.existence
      )}

    def InsertPartners(
      cids:     List[CallForProposals.Id],
      partners: List[CallForProposalsPartnerInput]
    ): Command[(cids.type, partners.type)] =
      sql"""
        INSERT INTO t_cfp_partner (
          c_cfp_id,
          c_partner,
          c_deadline
        ) VALUES ${(
          cfp_id  *:
          tag     *:
          core_timestamp
        ).values.list(cids.length * partners.length)}
      """.command
         .contramap {
           case (cids, partners) => cids.flatMap { cid =>
             partners.map { p => (cid, p.partner, p.deadline) }
           }
         }

    def DeletePartners(cids: List[CallForProposals.Id]): AppliedFragment =
      sql"""
        DELETE FROM t_cfp_partner
          WHERE c_cfp_id IN ${cfp_id.list(cids.length).values}
      """.apply(cids)

    def InsertInstruments(
      cids:        List[CallForProposals.Id],
      instruments: List[Instrument]
    ): Command[(cids.type, instruments.type)] =
      sql"""
        INSERT INTO t_cfp_instrument (
          c_cfp_id,
          c_instrument
        ) VALUES ${(cfp_id *: instrument).values.list(cids.length * instruments.length)}
      """.command
         .contramap {
           case (cids, instruments) => cids.flatMap(instruments.tupleLeft(_))
         }

    def DeleteInstruments(cids: List[CallForProposals.Id]): AppliedFragment =
      sql"""
        DELETE FROM t_cfp_instrument
          WHERE c_cfp_id IN ${cfp_id.list(cids.length).values}
      """.apply(cids)

    def UpdateCallsForProposals(
      SET:   CallForProposalsPropertiesInput.Edit,
      which: AppliedFragment
    ): AppliedFragment = {
      val upRaDec = List(
        SET.raLimit.map(_._1).foldPresent(sql"c_ra_start   = ${right_ascension.opt}"),
        SET.raLimit.map(_._2).foldPresent(sql"c_ra_end     = ${right_ascension.opt}"),
        SET.decLimit.map(_._1).foldPresent(sql"c_dec_start = ${declination.opt}"),
        SET.decLimit.map(_._2).foldPresent(sql"c_dec_end   = ${declination.opt}")
      ).flatMap(_.toList)

      val upActive = SET.active.map(_.fold(
        sql"c_active = tsrange($core_timestamp, upper(c_active))",
        sql"c_active = tsrange(lower(c_active), $core_timestamp)",
        (s, e) => sql"c_active = $timestamp_interval_tsrange"(TimestampInterval.between(s, e))
      ))

      val upExistence = sql"c_existence = $existence"
      val upSemester  = sql"c_semester  = $semester"
      val upType      = sql"c_type      = $cfp_type"

      val ups: Option[NonEmptyList[AppliedFragment]] =
        NonEmptyList.fromList(upRaDec ++ List(
          upActive,
          SET.existence.map(upExistence),
          SET.semester.map(upSemester),
          SET.cfpType.map(upType)
        ).flatten)

      def update(us: NonEmptyList[AppliedFragment]): AppliedFragment =
        void"UPDATE t_cfp "                                      |+|
          void"SET " |+| us.intercalate(void", ") |+| void" "    |+|
          void"WHERE t_cfp.c_cfp_id IN (" |+| which |+| void") " |+|
          void"RETURNING t_cfp.c_cfp_id"

       def selectOnly: AppliedFragment =
         void"SELECT c.c_cfp_id " |+|
           void"FROM t_cfp c "    |+|
           void"WHERE c.c_cfp_id IN (" |+| which |+| void")"

      ups.fold(selectOnly)(update)
    }
  }
}