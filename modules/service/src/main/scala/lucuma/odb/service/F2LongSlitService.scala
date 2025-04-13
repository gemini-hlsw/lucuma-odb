// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import lucuma.core.enums.F2Decker
import lucuma.core.enums.F2Disperser
import lucuma.core.enums.F2Filter
import lucuma.core.enums.F2Fpu
import lucuma.core.enums.F2ReadMode
import lucuma.core.enums.F2ReadoutMode
import lucuma.core.enums.F2Reads
import lucuma.core.model.Observation
import lucuma.odb.graphql.input.F2LongSlitInput
import lucuma.odb.sequence.f2.longslit.Config
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.F2Codecs.*
import skunk.*
import skunk.implicits.*

import Services.Syntax.*

trait F2LongSlitService[F[_]] {

  def select(
    which: List[Observation.Id]
  ): F[Map[Observation.Id, Config]]

  def insert(
    input: F2LongSlitInput.Create
  )(
    which: List[Observation.Id],
    xa:    Transaction[F]
  ): F[Unit]
}

object F2LongSlitService {

  def instantiate[F[_]: Concurrent](using Services[F]): F2LongSlitService[F] =

    new F2LongSlitService[F] {

      val f2LS: Decoder[F2LongSlitInput.Create] =
        (f2_disperser        *:
         f2_filter.opt       *:
         f2_fpu              *:
         f2_read_mode.opt    *:
         f2_reads.opt        *:
         f2_decker.opt       *:
         f2_readout_mode.opt
        ).to[F2LongSlitInput.Create]

      private def select[A](
        which:   List[Observation.Id],
        f:       NonEmptyList[Observation.Id] => AppliedFragment,
        decoder: Decoder[A]
      ): F[List[(Observation.Id, A)]] =
        NonEmptyList
          .fromList(which)
          .fold(Applicative[F].pure(List.empty)) { oids =>
            val af = f(oids)
            session.prepareR(af.fragment.query(observation_id *: decoder)).use { pq =>
              pq.stream(af.argument, chunkSize = 1024).compile.toList
            }
          }

      override def select(
        which: List[Observation.Id]
      ): F[Map[Observation.Id, Config]] =
        select(which, Statements.selectF2LongSlit, f2LS)
          .map(_.map { case (oid, f2) => (oid, f2.toObservingMode) }.toMap)

      override def insert(
        input: F2LongSlitInput.Create,
      )(
        which: List[Observation.Id],
        xa:    Transaction[F]
      ): F[Unit] =
        which.traverse { oid => session.exec(Statements.insertF2LongSlit(oid, input)) }.void

    }

  object Statements {

    def selectF2LongSlit(observationIds: NonEmptyList[Observation.Id]): AppliedFragment =
      sql"""
        SELECT
          ls.c_observation_id,
          ls.c_disperser,
          ls.c_filter,
          ls.c_fpu,
          ls.c_read_mode,
          ls.c_reads,
          ls.c_decker,
          ls.c_readout_mode
        FROM
          #t_flamingos_2_long_slit ls
        INNER JOIN t_observation ob ON ls.c_observation_id = ob.c_observation_id
      """(Void) |+|
      void"""
        WHERE
          ls.c_observation_id IN ("""                                     |+|
            observationIds.map(sql"$observation_id").intercalate(void",") |+|
          void")"

    val InsertF2LongSlit: Fragment[(
      Observation.Id          ,
      F2Disperser             ,
      Option[F2Filter] ,
      F2Fpu            ,
      Option[F2ReadMode] ,
      Option[F2Reads] ,
      Option[F2Decker] ,
      Option[F2ReadoutMode] ,
    )] =
      sql"""
        INSERT INTO t_flamingos_2_long_slit (
          c_observation_id,
          c_program_id,
          c_disperser,
          c_filter,
          c_fpu,
          c_read_mode,
          C_reads,
          c_decker,
          c_readout_mode
        )
        SELECT
          $observation_id,
          c_program_id,
          $f2_disperser,
          ${f2_filter.opt},
          $f2_fpu,
          ${f2_read_mode.opt},
          ${f2_reads.opt},
          ${f2_decker.opt},
          ${f2_readout_mode.opt}
        FROM t_observation
        WHERE c_observation_id = $observation_id
       """.contramap { (o, d, f, u, r, e, m, a) => (o, d, f, u, r, e, m, a, o)}

    def insertF2LongSlit(
      observationId: Observation.Id,
      input:         F2LongSlitInput.Create
    ): AppliedFragment =
      InsertF2LongSlit.apply(
        observationId            ,
        input.disperser          ,
        input.filter             ,
        input.fpu                ,
        input.explicitReadMode   ,
        input.explicitReads      ,
        input.explicitDecker     ,
        input.explicitReadoutMode
      )

  }
}
