// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Sync
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import lucuma.core.enums.GcalBaselineType
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.Instrument
import lucuma.core.enums.SmartGcalType
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.ExecutionSequence
import lucuma.core.model.sequence.GmosFpuMask
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.core.util.TimeSpan
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import skunk.*
import skunk.codec.numeric.int4
import skunk.implicits.*


trait SmartGcalService[F[_]] {

  def selectGmosNorth(gn: GmosNorth, sgt: SmartGcalType): F[List[(GmosNorth, Gcal)]]

  // Insertion is done by a flyway migration and not via this method.  The
  // insert here is for initializing a database for testing.
  def insertGmosNorth(
    id:           Int,
    stepOrder:    PosLong,
    disperser:    Option[GmosNorthGrating],
    filter:       Option[GmosNorthFilter],
    fpu:          Option[GmosNorthFpu],
    xBin:         GmosXBinning,
    yBin:         GmosYBinning,
    wavelength:   Option[BoundedInterval[Wavelength]],
    order:        Option[GmosGratingOrder],
    ampGain:      GmosAmpGain,
    gcal:         Gcal,
    count:        PosInt,
    baseline:     GcalBaselineType,
    exposureTime: TimeSpan
  ): F[Unit]

}

object SmartGcalService {

  def fromSession[F[_] : Sync](
    session: Session[F]
  ): SmartGcalService[F] =

    new SmartGcalService[F] {

      override def selectGmosNorth(
        gn:  GmosNorth,
        sgt: SmartGcalType
      ): F[List[(GmosNorth, Gcal)]] = {

        val af = Statements.selectGmosNorth(gn, sgt)
        session
          .prepareR(af.fragment.query(step_config_gcal ~ pos_int ~ time_span))
          .use(_.stream(af.argument, chunkSize = 16).compile.to(List))
          .map {
            _.flatMap { case ((gcal, count), exposureTime) =>
              List.fill(count.value)(
                GmosNorth.exposure.replace(exposureTime)(gn) -> gcal
              )
            }
          }
      }

      override def insertGmosNorth(
        id:           Int,
        stepOrder:    PosLong,
        disperser:    Option[GmosNorthGrating],
        filter:       Option[GmosNorthFilter],
        fpu:          Option[GmosNorthFpu],
        xBin:         GmosXBinning,
        yBin:         GmosYBinning,
        wavelength:   Option[BoundedInterval[Wavelength]],
        order:        Option[GmosGratingOrder],
        ampGain:      GmosAmpGain,
        gcal:         Gcal,
        count:        PosInt,
        baseline:     GcalBaselineType,
        exposureTime: TimeSpan
      ): F[Unit] =

        val gn  = session.executeCommand(
          Statements.InsertGmosNorth(
            Instrument.GmosNorth ~
            id                   ~
            stepOrder            ~
            disperser            ~
            filter               ~
            fpu                  ~
            xBin                 ~
            yBin                 ~
            wavelength           ~
            order                ~
            ampGain              ~
            exposureTime
          )
        ).void

        val gc  = session.executeCommand(
          Statements.InsertGcal(Instrument.GmosNorth ~ id ~ gcal ~ count ~ baseline)
        ).void

        for {
          _ <- gc
          _ <- gn
        } yield ()
    }


  object Statements {

    def selectGmosNorth(
      gn:  GmosNorth,
      sgt: SmartGcalType
    ): AppliedFragment = {
      val where = List(
        sql"s.c_disperser       IS NOT DISTINCT FROM ${gmos_north_grating.opt}"(gn.gratingConfig.map(_.grating)),
        sql"s.c_filter          IS NOT DISTINCT FROM ${gmos_north_filter.opt}"(gn.filter),
        sql"s.c_fpu             IS NOT DISTINCT FROM ${gmos_north_fpu.opt}"(gn.fpu.flatMap(GmosFpuMask.builtin.getOption)),
        sql"s.c_disperser_order IS NOT DISTINCT FROM ${gmos_disperser_order.opt}"(gn.gratingConfig.map(_.order)),
        sql"s.c_x_binning = ${gmos_x_binning}"(gn.readout.xBin),
        sql"s.c_y_binning = ${gmos_y_binning}"(gn.readout.yBin),
        sql"s.c_amp_gain  = ${gmos_amp_gain}"(gn.readout.ampGain),
        gn.gratingConfig.map(_.wavelength).fold(void"s.c_wavelength_range IS NULL")(
          sql"s.c_wavelength_range @> ${wavelength_pm}"
        ),
        sgt match {
          case SmartGcalType.Arc           => void"g.c_gcal_lamp_type = 'Arc'"
          case SmartGcalType.Flat          => void"g.c_gcal_lamp_type = 'Flat'"
          case SmartGcalType.DayBaseline   => void"g.c_gcal_baseline  = 'Day'"
          case SmartGcalType.NightBaseline => void"g.c_gcal_baseline  = 'Night'"
        }
      ).intercalate(void" AND ")

      void"""
        SELECT g.c_gcal_continuum,
               g.c_gcal_ar_arc,
               g.c_gcal_cuar_arc,
               g.c_gcal_thar_arc,
               g.c_gcal_xe_arc,
               g.c_gcal_filter,
               g.c_gcal_diffuser,
               g.c_gcal_shutter,
               g.c_gcal_step_count,
               s.c_exposure_time
          FROM t_smart_gmos_north s
          JOIN t_gcal             g ON s.c_instrument = g.c_instrument
                                   AND s.c_gcal_id    = g.c_gcal_id
         WHERE """ |+| where |+| void" ORDER BY s.c_step_order"
    }

    val InsertGcal: Fragment[
      Instrument ~
        Int      ~
        Gcal     ~
        PosInt   ~
        GcalBaselineType
    ] =
      sql"""
        INSERT INTO t_gcal (
          c_instrument,
          c_gcal_id,
          c_gcal_continuum,
          c_gcal_ar_arc,
          c_gcal_cuar_arc,
          c_gcal_thar_arc,
          c_gcal_xe_arc,
          c_gcal_filter,
          c_gcal_diffuser,
          c_gcal_shutter,
          c_gcal_step_count,
          c_gcal_baseline
        ) SELECT
          $instrument,
          $int4,
          $step_config_gcal,
          $pos_int,
          $gcal_baseline
      """

    val InsertGmosNorth: Fragment[
      Instrument                          ~
      Int                                 ~
      PosLong                             ~
      Option[GmosNorthGrating]            ~
      Option[GmosNorthFilter]             ~
      Option[GmosNorthFpu]                ~
      GmosXBinning                        ~
      GmosYBinning                        ~
      Option[BoundedInterval[Wavelength]] ~
      Option[GmosGratingOrder]            ~
      GmosAmpGain                         ~
      TimeSpan
    ] =
      sql"""
        INSERT INTO t_smart_gmos_north (
          c_instrument,
          c_gcal_id,
          c_step_order,
          c_disperser,
          c_filter,
          c_fpu,
          c_x_binning,
          c_y_binning,
          c_wavelength_range,
          c_disperser_order,
          c_amp_gain,
          c_exposure_time
        ) SELECT
          $instrument,
          $int4,
          $pos_long,
          ${gmos_north_grating.opt},
          ${gmos_north_filter.opt},
          ${gmos_north_fpu.opt},
          $gmos_x_binning,
          $gmos_y_binning,
          ${wavelength_pm_range.opt},
          ${gmos_disperser_order.opt},
          $gmos_amp_gain,
          $time_span
      """
  }
}

