// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Sync
import cats.syntax.foldable.*
import lucuma.core.enums.SmartGcalType
import lucuma.core.model.sequence.GmosFpuMask
import lucuma.core.model.sequence.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import skunk.*
import skunk.implicits.*

trait SmartGcalService[F[_]] {

  def selectGmosNorth(gn: GmosNorth, sgt: SmartGcalType): F[List[(GmosNorth, Gcal)]]
   // placeholder
}

object SmartGcalService {

  def fromSession[F[_]: Sync](
    session: Session[F]
  ): SmartGcalService[F] =

    new SmartGcalService[F] {
      def selectGmosNorth(gn: GmosNorth, sgt: SmartGcalType): F[List[(GmosNorth, Gcal)]] =
        ???

    }


  object Statements {

    def select(
      gn:  GmosNorth,
      sgt: SmartGcalType
    ): AppliedFragment =

      val where = List(
        gn.gratingConfig.map(_.grating).fold(void"s.c_disperser IS NULL")(sql"s.c_disperser = ${gmos_north_grating}"),
        gn.filter.fold(void"s.c_filter IS NULL")(sql"s.c_filter = ${gmos_north_filter}"),
        gn.fpu.flatMap(GmosFpuMask.builtin.getOption).fold(void"s.c_fpu IS NULL")(sql"s.c_fpu = ${gmos_north_fpu}"),
        sql"s.c_x_binning = ${gmos_x_binning}"(gn.readout.xBin),
        sql"s.c_y_binning = ${gmos_y_binning}"(gn.readout.yBin),
        sql"s.c_amp_gain  = ${gmos_amp_gain}"(gn.readout.ampGain),
        sgt match {
          case SmartGcalType.Arc           => void"AND g.gcal_lamp_type = 'Arc'"
          case SmartGcalType.Flat          => void"AND g.gcal_lamp_type = 'Flat'"
          case SmartGcalType.DayBaseline   => void"AND g.gcal_baseline  = 'Day'"
          case SmartGcalType.NightBaseline => void"AND g.gcal_baseline  = 'Night'"
        }
      ).foldMap(void" AND " |+| _)

      sql"""
        SELECT g.c_gcal_continuum
               g.c_gcal_ar_arc
               g.c_gcal_cuar_arc
               g.c_gcal_thar_arc
               g.c_gcal_xe_arc
               g.c_gcal_filter
               g.c_gcal_diffuser
               g.c_gcal_shutter
               g.c_gcal_step_count
               s.c_exposure_time
          FROM t_smart_gmos_north s
          JOIN t_gcal             g ON s.c_instrument = g.c_instrument
                                   AND s.c_gcal_id    = g.c_gcal_id
         WHERE s.c_disperser IS NOT DISTINCT FROM ${gmos_north_grating.opt}
           AND s.c_filter    IS NOT DISTINCT FROM ${gmos_north_filter.opt}
           AND s.c_fpu       IS NOT DISTNICT FROM ${gmos_north_fpu.opt}
           AND s.c_x_binning = $gmos_x_binning
           AND s.c_y_binning = $gmos_y_binning
           AND s.c_amp_gain  = $gmos_amp_gain
      ORDER BY s.c_step_order
      """.apply(
        gn.gratingConfig.map(_.grating)               ~
        gn.filter                                     ~
        gn.fpu.flatMap(GmosFpuMask.builtin.getOption) ~
        gn.readout.xBin                               ~
        gn.readout.yBin                               ~
        gn.readout.ampGain
      )

//           AND s.c_wavelength_range @> ${wavelength_pm}
//           AND s.c_disperser_order   = ${gmos_disperser_order}


  }

}


