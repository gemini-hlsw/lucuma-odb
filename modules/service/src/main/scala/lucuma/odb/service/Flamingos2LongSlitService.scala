// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import lucuma.core.enums.Flamingos2Decker
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.Flamingos2ReadoutMode
import lucuma.core.enums.Flamingos2Reads
import lucuma.core.model.Observation
import lucuma.core.model.SourceProfile
import lucuma.odb.format.spatialOffsets.*
import lucuma.odb.graphql.input.Flamingos2LongSlitInput
import lucuma.odb.sequence.flamingos2.longslit.Config
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.Flamingos2Codecs.*
import skunk.*
import skunk.codec.text.text
import skunk.implicits.*

import Services.Syntax.*

trait Flamingos2LongSlitService[F[_]] {

  def select(
    which: List[Observation.Id]
  ): F[Map[Observation.Id, SourceProfile => Config]]

  def insert(input: Flamingos2LongSlitInput.Create)(
    which: List[Observation.Id])(using Transaction[F]): F[Unit]

  def delete(which: List[Observation.Id])(using Transaction[F]): F[Unit]

  def update(SET: Flamingos2LongSlitInput.Edit)(
    which: List[Observation.Id])(using Transaction[F]): F[Unit]

  def clone(originalId: Observation.Id, newId: Observation.Id): F[Unit]
}

object Flamingos2LongSlitService {

  def instantiate[F[_]: {Concurrent as F, Services}]: Flamingos2LongSlitService[F] =

    new Flamingos2LongSlitService[F] {

      val f2LS: Decoder[Flamingos2LongSlitInput.Create] =
        (flamingos_2_disperser        *:
         flamingos_2_filter           *:
         flamingos_2_fpu              *:
         flamingos_2_read_mode.opt    *:
         flamingos_2_reads.opt        *:
         flamingos_2_decker.opt       *:
         flamingos_2_readout_mode.opt *:
         text.opt
        ).emap { case (disperser, filter, fpu, readMode, reads, decker, readoutMode, spatialOffsetsText) =>
          spatialOffsetsText.traverse(so => OffsetsQFormat.getOption(so).toRight(s"Could not parse '$so' as a spatial offsets list.")).map(spatialOffsets =>
            Flamingos2LongSlitInput.Create(disperser, filter, fpu, readMode, reads, decker, readoutMode, spatialOffsets)
          )
        }

      private def select[A](
        which:   List[Observation.Id],
        f:       NonEmptyList[Observation.Id] => AppliedFragment,
        decoder: Decoder[A]
      ): F[List[(Observation.Id, A)]] =
        NonEmptyList
          .fromList(which)
          .fold(List.empty.pure[F]) { oids =>
            val af = f(oids)
            session.prepareR(af.fragment.query(observation_id *: decoder)).use { pq =>
              pq.stream(af.argument, chunkSize = 1024).compile.toList
            }
          }

      override def select(
        which: List[Observation.Id]
      ): F[Map[Observation.Id, SourceProfile => Config]] =
        select(which, Statements.selectFlamingos2LongSlit, f2LS)
          .map(_.map { case (oid, f2) => (oid, (_: SourceProfile) => f2.toObservingMode) }.toMap)

      override def insert(
        input: Flamingos2LongSlitInput.Create,
      )(which: List[Observation.Id])(using Transaction[F]): F[Unit] =
        which.traverse { oid => session.exec(Statements.insertF2LongSlit(oid, input)) }.void

      def delete(which: List[Observation.Id] )(using Transaction[F]): F[Unit] =
        Statements.deleteF2(which).fold(F.unit)(session.exec)

      def update(SET: Flamingos2LongSlitInput.Edit)(
        which: List[Observation.Id])(using Transaction[F]): F[Unit] =
        Statements.updateF2LongSlit(SET, which).fold(F.unit)(session.exec)

      def clone(originalId: Observation.Id, newId: Observation.Id): F[Unit] =
        session.exec(Statements.cloneF2(originalId, newId))
    }

  object Statements {

    def selectFlamingos2LongSlit(observationIds: NonEmptyList[Observation.Id]): AppliedFragment =
      sql"""
        SELECT
          ls.c_observation_id,
          ls.c_disperser,
          ls.c_filter,
          ls.c_fpu,
          ls.c_read_mode,
          ls.c_reads,
          ls.c_decker,
          ls.c_readout_mode,
          ls.c_spatial_offsets
        FROM
          t_flamingos_2_long_slit ls
        INNER JOIN t_observation ob ON ls.c_observation_id = ob.c_observation_id
      """(Void) |+|
      void"""
        WHERE
          ls.c_observation_id IN ("""                                     |+|
            observationIds.map(sql"$observation_id").intercalate(void",") |+|
          void")"

    val InsertF2LongSlit: Fragment[(
      Observation.Id               ,
      Flamingos2Disperser          ,
      Flamingos2Filter             ,
      Flamingos2Fpu                ,
      Option[Flamingos2ReadMode]   ,
      Option[Flamingos2Reads]      ,
      Option[Flamingos2Decker]     ,
      Option[Flamingos2ReadoutMode],
      Option[String]               ,
      Flamingos2Disperser          ,
      Flamingos2Filter             ,
      Flamingos2Fpu
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
          c_readout_mode,
          c_spatial_offsets,
          c_initial_disperser,
          c_initial_filter,
          c_initial_fpu
        )
        SELECT
          $observation_id,
          c_program_id,
          $flamingos_2_disperser,
          $flamingos_2_filter,
          $flamingos_2_fpu,
          ${flamingos_2_read_mode.opt},
          ${flamingos_2_reads.opt},
          ${flamingos_2_decker.opt},
          ${flamingos_2_readout_mode.opt},
          ${text.opt},
          $flamingos_2_disperser,
          $flamingos_2_filter,
          $flamingos_2_fpu
        FROM t_observation
        WHERE c_observation_id = $observation_id
       """.contramap { (o, d, f, u, r, e, m, a, s, id, ii, iu) => (o, d, f, u, r, e, m, a, s, id, ii, iu, o)}

    def insertF2LongSlit(
      observationId: Observation.Id,
      input:         Flamingos2LongSlitInput.Create
    ): AppliedFragment =
      InsertF2LongSlit.apply(
        observationId                ,
        input.disperser              ,
        input.filter                 , 
        input.fpu                    ,
        input.explicitReadMode       ,
        input.explicitReads          ,
        input.explicitDecker         ,
        input.explicitReadoutMode    ,
        input.formattedSpatialOffsets,
        input.disperser              ,
        input.filter                 ,
        input.fpu                    ,
      )

    def deleteF2(which: List[Observation.Id]): Option[AppliedFragment] =
      NonEmptyList.fromList(which).map { oids =>
        void"DELETE FROM ONLY t_flamingos_2_long_slit " |+|
          void"WHERE " |+| observationIdIn(oids)
      }

    private def f2Updates(input: Flamingos2LongSlitInput.Edit): Option[NonEmptyList[AppliedFragment]] = {

      val upDisperser      = sql"c_disperser       = $flamingos_2_disperser"
      val upFilter         = sql"c_filter          = $flamingos_2_filter"
      val upFpu            = sql"c_fpu             = $flamingos_2_fpu"
      val upReadMode       = sql"c_read_mode       = ${flamingos_2_read_mode.opt}"
      val upReads          = sql"c_reads           = ${flamingos_2_reads.opt}"
      val upDecker         = sql"c_decker          = ${flamingos_2_decker.opt}"
      val upReadoutMode    = sql"c_readout_mode    = ${flamingos_2_readout_mode.opt}"
      val upSpatialOffsets = sql"c_spatial_offsets = ${text.opt}"

      val ups: List[AppliedFragment] =
        List(
          input.disperser.map(upDisperser),
          input.filter.map(upFilter),
          input.fpu.map(upFpu),
          input.explicitReadMode.toOptionOption.map(upReadMode),
          input.explicitReads.toOptionOption.map(upReads),
          input.explicitDecker.toOptionOption.map(upDecker),
          input.explicitReadoutMode.toOptionOption.map(upReadoutMode),
          input.formattedSpatialOffsets.toOptionOption.map(upSpatialOffsets)
        ).flatten

      NonEmptyList.fromList(ups)
    }

    def updateF2LongSlit(
      SET:   Flamingos2LongSlitInput.Edit,
      which: List[Observation.Id]
    ): Option[AppliedFragment] =
      for {
        us   <- f2Updates(SET)
        oids <- NonEmptyList.fromList(which)
      } yield
        void"UPDATE t_flamingos_2_long_slit " |+|
          void"SET " |+| us.intercalate(void", ") |+| void" " |+|
          void"WHERE " |+| observationIdIn(oids)

    def cloneF2(originalId: Observation.Id, newId: Observation.Id): AppliedFragment =
      sql"""
      INSERT INTO t_flamingos_2_long_slit (
        c_observation_id,
        c_program_id,
        c_observing_mode_type,
        c_disperser,
        c_filter,
        c_fpu,
        c_read_mode,
        c_reads,
        c_decker,
        c_decker_default,
        c_readout_mode,
        c_readout_mode_default,
        c_spatial_offsets,
        c_initial_disperser,
        c_initial_filter,
        c_initial_fpu
      )
      SELECT
        $observation_id,
        (SELECT c_program_id FROM t_observation WHERE c_observation_id = $observation_id) AS c_program_id,
        c_observing_mode_type,
        c_disperser,
        c_filter,
        c_fpu,
        c_read_mode,
        c_reads,
        c_decker,
        c_decker_default,
        c_readout_mode,
        c_readout_mode_default,
        c_spatial_offsets,
        c_initial_disperser,
        c_initial_filter,
        c_initial_fpu
      FROM t_flamingos_2_long_slit
      WHERE c_observation_id = $observation_id
      """.apply(newId, newId, originalId)
  }
}
