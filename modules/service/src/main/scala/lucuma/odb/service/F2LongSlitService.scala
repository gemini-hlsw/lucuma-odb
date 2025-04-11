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
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.odb.graphql.input.F2LongSlitInput
import lucuma.odb.graphql.input.GmosLongSlitInput
import lucuma.odb.sequence.f2.longslit.Config
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.F2Codecs.*
import lucuma.odb.util.GmosCodecs.*
import skunk.*
import skunk.codec.text.text
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
         f2_decker.opt       *:
         f2_readout_mode.opt *:
         f2_reads.opt        //*:
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
          ls.c_decker,
          ls.c_readout_mode,
          ls.c_reads
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
        observationId                          ,
          input.disperser                        ,
          input.filter                         ,
          input.fpu                            ,
          input.explicitReadMode,
          input.explicitReads                            ,
          input.explicitDecker                            ,
          input.explicitReadoutMode                            ,
      )

    private def observationIdIn(
      oids: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      void"c_observation_id IN (" |+|
        oids.map(sql"$observation_id").intercalate(void", ") |+|
      void")"

    def deleteGmosNorthLongSlit(
      which: List[Observation.Id]
    ): Option[AppliedFragment] =
      NonEmptyList.fromList(which).map { oids =>
        void"DELETE FROM ONLY t_gmos_north_long_slit " |+|
          void"WHERE " |+| observationIdIn(oids)
      }

    def deleteGmosSouthLongSlit(
      which: List[Observation.Id]
    ): Option[AppliedFragment] =
      NonEmptyList.fromList(which).map { oids =>
        void"DELETE FROM ONLY t_gmos_south_long_slit " |+|
          void"WHERE " |+| observationIdIn(oids)
      }

    def commonUpdates(
      input: GmosLongSlitInput.Edit.Common
    ): List[AppliedFragment] = {
      val upCentralλ    = sql"c_central_wavelength = $wavelength_pm"
      val upXBin        = sql"c_xbin               = ${gmos_x_binning.opt}"
      val upYBin        = sql"c_ybin               = ${gmos_y_binning.opt}"
      val upAmpReadMode = sql"c_amp_read_mode      = ${gmos_amp_read_mode.opt}"
      val upAmpGain     = sql"c_amp_gain           = ${gmos_amp_gain.opt}"
      val upRoi         = sql"c_roi                = ${gmos_roi.opt}"
      val upλDithers    = sql"c_wavelength_dithers = ${text.opt}"
      val upOffsets     = sql"c_spatial_offsets    = ${text.opt}"

      List(
        input.centralWavelength.map(upCentralλ),
        input.explicitXBin.toOptionOption.map(upXBin),
        input.explicitYBin.toOptionOption.map(upYBin),
        input.explicitAmpReadMode.toOptionOption.map(upAmpReadMode),
        input.explicitAmpGain.toOptionOption.map(upAmpGain),
        input.explicitRoi.toOptionOption.map(upRoi),
        input.formattedλDithers.toOptionOption.map(upλDithers),
        input.formattedSpatialOffsets.toOptionOption.map(upOffsets)
      ).flatten
    }

    def gmosNorthUpdates(
      input: GmosLongSlitInput.Edit.North
    ): Option[NonEmptyList[AppliedFragment]] = {

      val upGrating     = sql"c_grating = $gmos_north_grating"
      val upFilter      = sql"c_filter  = ${gmos_north_filter.opt}"
      val upFpu         = sql"c_fpu     = $gmos_north_fpu"

      val ups: List[AppliedFragment] =
        List(
          input.grating.map(upGrating),
          input.filter.toOptionOption.map(upFilter),
          input.fpu.map(upFpu),
        ).flatten ++ commonUpdates(input.common)

      NonEmptyList.fromList(ups)
    }

    def updateGmosNorthLongSlit(
      SET:   GmosLongSlitInput.Edit.North,
      which: List[Observation.Id]
    ): Option[AppliedFragment] =

      for {
        us   <- gmosNorthUpdates(SET)
        oids <- NonEmptyList.fromList(which)
      } yield
        void"UPDATE t_gmos_north_long_slit " |+|
          void"SET " |+| us.intercalate(void", ") |+| void" " |+|
          void"WHERE " |+| observationIdIn(oids)

    def gmosSouthUpdates(
      input: GmosLongSlitInput.Edit.South
    ): Option[NonEmptyList[AppliedFragment]] = {

      val upGrating     = sql"c_grating = $gmos_south_grating"
      val upFilter      = sql"c_filter  = ${gmos_south_filter.opt}"
      val upFpu         = sql"c_fpu     = $gmos_south_fpu"

      val ups: List[AppliedFragment] =
        List(
          input.grating.map(upGrating),
          input.filter.toOptionOption.map(upFilter),
          input.fpu.map(upFpu),
        ).flatten ++ commonUpdates(input.common)

      NonEmptyList.fromList(ups)
    }

    def updateGmosSouthLongSlit(
      SET:   GmosLongSlitInput.Edit.South,
      which: List[Observation.Id]
    ): Option[AppliedFragment] =

      for {
        us   <- gmosSouthUpdates(SET)
        oids <- NonEmptyList.fromList(which)
      } yield
        void"UPDATE t_gmos_south_long_slit " |+|
          void"SET " |+| us.intercalate(void", ") |+| void" " |+|
          void"WHERE " |+| observationIdIn(oids)

    private def cloneGmosLongSlit(
      table: String,
      originalId: Observation.Id,
      newId: Observation.Id
    ): AppliedFragment =
      sql"""
      INSERT INTO #$table (
        c_observation_id,
        c_program_id,
        c_observing_mode_type,
        c_grating,
        c_filter,
        c_fpu,
        c_central_wavelength,
        c_xbin,
        c_ybin,
        c_amp_read_mode,
        c_amp_gain,
        c_roi,
        c_wavelength_dithers,
        c_spatial_offsets,
        c_initial_grating,
        c_initial_filter,
        c_initial_fpu,
        c_initial_central_wavelength
      )
      SELECT
        $observation_id,
        (SELECT c_program_id FROM t_observation WHERE c_observation_id = $observation_id) AS c_program_id,
        c_observing_mode_type,
        c_grating,
        c_filter,
        c_fpu,
        c_central_wavelength,
        c_xbin,
        c_ybin,
        c_amp_read_mode,
        c_amp_gain,
        c_roi,
        c_wavelength_dithers,
        c_spatial_offsets,
        c_initial_grating,
        c_initial_filter,
        c_initial_fpu,
        c_initial_central_wavelength
      FROM #$table
      WHERE c_observation_id = $observation_id
      """.apply(newId, newId, originalId)

    def cloneGmosNorthLongSlit(originalId: Observation.Id, newId: Observation.Id): AppliedFragment =
      cloneGmosLongSlit("t_gmos_north_long_slit", originalId, newId)

    def cloneGmosSouthLongSlit(originalId: Observation.Id, newId: Observation.Id): AppliedFragment =
      cloneGmosLongSlit("t_gmos_south_long_slit", originalId, newId)

  }
}
