// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import grackle.Result
import grackle.syntax.*
import lucuma.core.enums.Instrument
import lucuma.core.model.CallForProposals
import lucuma.odb.graphql.input.CallForProposalsPartnerInput
import lucuma.odb.graphql.input.CallForProposalsPropertiesInput
import lucuma.odb.graphql.input.CreateCallForProposalsInput
import lucuma.odb.util.Codecs.*
import skunk.Command
import skunk.Query
import skunk.Transaction
import skunk.implicits.*

import Services.Syntax.*

trait CallForProposalsService[F[_]] {

  def createCallForProposals(
    input: CreateCallForProposalsInput
  )(using Transaction[F], Services.StaffAccess): F[Result[CallForProposals.Id]]

}

object CallForProposalsService {

  def instantiate[F[_]: Concurrent](using Services[F]): CallForProposalsService[F] =
    new CallForProposalsService[F] {

      override def createCallForProposals(
        input: CreateCallForProposalsInput
      )(using Transaction[F], Services.StaffAccess): F[Result[CallForProposals.Id]] = {
        val partners    = input.SET.partners
        val instruments = input.SET.instruments
        (for {
          cid <- session.unique(Statements.InsertCallForProposals)(input.SET)
          _   <- session
                   .prepareR(Statements.InsertPartners(partners))
                   .use(_.execute(cid, partners))
                   .whenA(partners.nonEmpty)
          _   <- session
                   .prepareR(Statements.InsertInstruments(instruments))
                   .use(_.execute(cid, instruments))
                   .whenA(instruments.nonEmpty)
        } yield cid).map(_.success)
      }
    }

  object Statements {

    val InsertCallForProposals: Query[CallForProposalsPropertiesInput.Create, CallForProposals.Id] =
      sql"""
        INSERT INTO t_cfp (
          c_status,
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
          $cfp_status,
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
        input.status,
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
      partners: List[CallForProposalsPartnerInput]
    ): Command[(CallForProposals.Id, partners.type)] =
      sql"""
        INSERT INTO t_cfp_partner (
          c_cfp_id,
          c_partner,
          c_deadline
        ) VALUES ${(
          cfp_id  *:
          tag     *:
          core_timestamp
        ).values.list(partners.length)}
      """.command
         .contramap {
           case (cid, partners) => partners.map { p =>
             (cid, p.partner, p.deadline)
           }
         }

    def InsertInstruments(
      instruments: List[Instrument]
    ): Command[(CallForProposals.Id, instruments.type)] =
      sql"""
        INSERT INTO t_cfp_instrument (
          c_cfp_id,
          c_instrument
        ) VALUES ${(cfp_id *: instrument).values.list(instruments.length)}
      """.command
         .contramap {
           case (cid, instruments) => instruments.tupleLeft(cid)
         }
  }
}