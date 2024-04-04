// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.functor.*
import grackle.Result
import grackle.syntax.*
import lucuma.odb.data.CallForProposals
import lucuma.odb.graphql.input.CallForProposalsPropertiesInput
import lucuma.odb.graphql.input.CreateCallForProposalsInput
import lucuma.odb.util.Codecs.*
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
      )(using Transaction[F], Services.StaffAccess): F[Result[CallForProposals.Id]] =
        session
          .unique(Statements.InsertCallForProposals)(input.SET)
          .map(_.success)

    }

  object Statements {

    val InsertCallForProposals: Query[CallForProposalsPropertiesInput.Create, CallForProposals.Id] =
      sql"""
        INSERT INTO t_cfp (
          c_status,
          c_type,
          c_semester,
          c_active,
          c_existence
        )
        SELECT
          $cfp_status,
          $cfp_type,
          $semester,
          $timestamp_interval_tsrange,
          $existence
        RETURNING
          c_cfp_id
      """.query(cfp_id).contramap { input => (
        input.status,
        input.cfpType,
        input.semester,
        input.active,
        input.existence
      )}

  }
}