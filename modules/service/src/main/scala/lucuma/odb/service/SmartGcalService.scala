// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Sync
import cats.syntax.foldable.*
import cats.syntax.functor.*
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

}

object SmartGcalService {

  def fromSession[F[_]: Sync](
    session: Session[F]
  ): SmartGcalService[F] =

    new SmartGcalService[F] {
      override def selectGmosNorth(
        gn: GmosNorth,
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

    }


  object Statements {

    def selectGmosNorth(
      gn:  GmosNorth,
      sgt: SmartGcalType
    ): AppliedFragment =

      val where = List(
        sql"s.c_disperser       IS NOT DISTINCT FROM ${gmos_north_grating.opt}"(gn.gratingConfig.map(_.grating)),
        sql"s.c_filter          IS NOT DISTINCT FROM ${gmos_north_filter.opt}"(gn.filter),
        sql"s.c_fpu             IS NOT DISTNICT FROM ${gmos_north_fpu.opt}"(gn.fpu.flatMap(GmosFpuMask.builtin.getOption)),
        sql"s.c_disperser_order IS NOT DISTINCT FROM ${gmos_disperser_order.opt}"(gn.gratingConfig.map(_.order)),
        sql"s.c_x_binning = ${gmos_x_binning}"(gn.readout.xBin),
        sql"s.c_y_binning = ${gmos_y_binning}"(gn.readout.yBin),
        sql"s.c_amp_gain  = ${gmos_amp_gain}"(gn.readout.ampGain),
        gn.gratingConfig.map(_.wavelength).fold(void"s.c_wavelength_range IS NULL")(
          sql"s.c_wavelength_range @> ${wavelength_pm}"
        ),
        sgt match {
          case SmartGcalType.Arc           => void"g.gcal_lamp_type = 'Arc'"
          case SmartGcalType.Flat          => void"g.gcal_lamp_type = 'Flat'"
          case SmartGcalType.DayBaseline   => void"g.gcal_baseline  = 'Day'"
          case SmartGcalType.NightBaseline => void"g.gcal_baseline  = 'Night'"
        }
      ).foldMap(void" AND " |+| _)

      void"""
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
         WHERE """ |+| where |+| void" ORDER BY s.c_step_order"

  }

}


