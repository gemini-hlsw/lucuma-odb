// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.GcalBaselineType
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosBinning
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.Instrument
import lucuma.core.enums.SmartGcalType
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig as Flamingos2
import lucuma.core.model.sequence.ghost.GhostDetector
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig as Igrins2
import lucuma.core.util.TimeSpan
import lucuma.odb.service.Services.GuestAccess
import lucuma.odb.service.Services.SuperUserAccess
import lucuma.odb.smartgcal.data.Flamingos2.TableKey as Flamingos2SearchKey
import lucuma.odb.smartgcal.data.Flamingos2.TableRow as Flamingos2TableRow
import lucuma.odb.smartgcal.data.Ghost.GhostUpdate
import lucuma.odb.smartgcal.data.Ghost.SearchKey as GhostSearchKey
import lucuma.odb.smartgcal.data.Ghost.TableRow as GhostTableRow
import lucuma.odb.smartgcal.data.Gmos.SearchKey.North as GmosNorthSearchKey
import lucuma.odb.smartgcal.data.Gmos.SearchKey.South as GmosSouthSearchKey
import lucuma.odb.smartgcal.data.Gmos.TableRow.North as GmosNorthTableRow
import lucuma.odb.smartgcal.data.Gmos.TableRow.South as GmosSouthTableRow
import lucuma.odb.smartgcal.data.Igrins2.TableKey as Igrins2SearchKey
import lucuma.odb.smartgcal.data.Igrins2.TableRow as Igrins2TableRow
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.Flamingos2Codecs.*
import lucuma.odb.util.GhostCodecs.*
import lucuma.odb.util.GmosCodecs.*
import skunk.*
import skunk.codec.all.*
import skunk.implicits.*

import Services.Syntax.*

trait SmartGcalService[F[_]]:

  /**
   * Selects calibration information corresponding to the given search key and
   * type.  There can be multiple steps per key.
   *
   * @param ghost Ghost search information
   * @param sgt SmartGcal type of interest
   *
   * @return list of tuples, each corresponding to a calibration step; each
   *         tuple contains an update to the dynamic config to apply (e.g., to
   *         set its exposure time) and the GCAL unit configuration to use
   */
  def selectGhost(
    ghost: GhostSearchKey,
    sgt:   SmartGcalType
  )(using GuestAccess): F[List[(GhostDynamicConfig => GhostDynamicConfig, Gcal)]]

  /**
   * Selects calibration information corresponding to the given search key and
   * type.  There can be multiple steps per key.
   *
   * @param gn  GMOS North search information
   * @param sgt SmartGcal type of interest
   *
   * @return list of tuples, each corresponding to a calibration step; each
   *         tuple contains an update to the dynamic config to apply (e.g., to
   *         set its exposure time) and the GCAL unit configuration to use
   */
  def selectGmosNorth(
    gn:  GmosNorthSearchKey,
    sgt: SmartGcalType
  )(using GuestAccess): F[List[(GmosNorth => GmosNorth, Gcal)]]

  /**
   * Selects calibration information corresponding to the given search key and
   * type.  There can be multiple steps per key.
   *
   * @param gn  GMOS South search information
   * @param sgt SmartGcal type of interest
   *
   * @return list of tuples, each corresponding to a calibration step; each
   *         tuple contains an update to the dynamic config to apply (e.g., to
   *         set its exposure time) and the GCAL unit configuration to use
   */
  def selectGmosSouth(
    gn:  GmosSouthSearchKey,
    sgt: SmartGcalType
  )(using GuestAccess): F[List[(GmosSouth => GmosSouth, Gcal)]]

  /**
   * Selects calibration information corresponding to the given search key and
   * type.  There can be multiple steps per key.
   *
   * @param f2  Flamingos 2 search information
   * @param sgt SmartGcal type of interest
   *
   * @return list of tuples, each corresponding to a calibration step; each
   *         tuple contains an update to the dynamic config to apply (e.g., to
   *         set its exposure time) and the GCAL unit configuration to use
   */
  def selectFlamingos2(
    f2:  Flamingos2SearchKey,
    sgt: SmartGcalType
  )(using GuestAccess): F[List[(Flamingos2 => Flamingos2, Gcal)]]

  def selectIgrins2(
    key: Igrins2SearchKey.type,
    sgt: SmartGcalType
  )(using GuestAccess): F[List[(Igrins2 => Igrins2, Gcal)]]

  // N.B. Insertion is done by a flyway migration and not via the insert methods.
  // The insert here is intended for initializing a database for testing.

  def insertGhost(
    id:   Int,
    line: PosLong,
    row:  GhostTableRow
  )(using SuperUserAccess): F[Unit]

  def insertGmosNorth(
    id:  Int,
    row: GmosNorthTableRow
  )(using SuperUserAccess): F[Unit]

  def insertGmosSouth(
    id:  Int,
    row: GmosSouthTableRow
  )(using SuperUserAccess): F[Unit]

  def insertFlamingos2(
    id:  Int,
    row: Flamingos2TableRow
  )(using SuperUserAccess): F[Unit]

  def insertIgrins2(
    id:  Int,
    row: Igrins2TableRow
  )(using SuperUserAccess): F[Unit]


object SmartGcalService:

  def instantiate[F[_] : Concurrent](using Services[F]): SmartGcalService[F] =
    new SmartGcalService[F]:

      override def selectGhost(
        ghost: GhostSearchKey,
        sgt:   SmartGcalType
      )(using GuestAccess): F[List[(GhostDynamicConfig => GhostDynamicConfig, Gcal)]] =
        def update(
          g: GhostDynamicConfig,
          c: GhostUpdate
        ): GhostDynamicConfig =
          val redʹ  = g.red.value.copy(exposureTime = c.redExposureTime, exposureCount = c.redExposureCount)
          val blueʹ = g.blue.value.copy(exposureTime = c.blueExposureTime, exposureCount = c.blueExposureCount)
          g.copy(red = GhostDetector.Red(redʹ), blue = GhostDetector.Blue(blueʹ))

        session
          .execute(Statements.selectGhost(sgt))(ghost)
          .map:
            _.flatMap: (gcal, count, ghostConfig) =>
              List.fill(count.value)((g => update(g, ghostConfig), gcal))

      override def selectGmosNorth(
        gn:  GmosNorthSearchKey,
        sgt: SmartGcalType
      )(using GuestAccess): F[List[(GmosNorth => GmosNorth, Gcal)]] =
        selectGcal(Statements.selectGmosNorth(gn, sgt)) { exposureTime =>
          GmosNorth.exposure.replace(exposureTime)
        }

      override def selectGmosSouth(
        gs:  GmosSouthSearchKey,
        sgt: SmartGcalType
      )(using GuestAccess): F[List[(GmosSouth => GmosSouth, Gcal)]] =
        selectGcal(Statements.selectGmosSouth(gs, sgt)) { exposureTime =>
          GmosSouth.exposure.replace(exposureTime)
        }

      private def selectGcal[D](
        af: AppliedFragment
      )(
        f: TimeSpan => D => D
      ): F[List[(D => D, Gcal)]] =
        session
          .prepareR(af.fragment.query(step_config_gcal ~ int4_pos ~ time_span))
          .use(_.stream(af.argument, chunkSize = 16).compile.to(List))
          .map {
            _.flatMap { case ((gcal, count), exposureTime) =>
              List.fill(count.value)(f(exposureTime) -> gcal)
            }
          }

      def selectFlamingos2(
        f2:  Flamingos2SearchKey,
        sgt: SmartGcalType
      )(using GuestAccess): F[List[(Flamingos2 => Flamingos2, Gcal)]] =
        selectGcal(Statements.selectF2(f2, sgt)) { exposureTime =>
          Flamingos2.exposure.replace(exposureTime)
        }

      def selectIgrins2(
        key: Igrins2SearchKey.type,
        sgt: SmartGcalType
      )(using GuestAccess): F[List[(Igrins2 => Igrins2, Gcal)]] =
        selectGcal(Statements.selectIgrins2(key, sgt)): exposureTime =>
          Igrins2.exposure.replace(exposureTime)

      override def insertGhost(
        id:   Int,
        line: PosLong,
        row:  GhostTableRow
      )(using SuperUserAccess): F[Unit] =
        val insertInstRow =
          session.execute(Statements.InsertGhost)(id, line, row).void

        val insertGcalRow =
          session.executeCommand(
            Statements.InsertGcal(Instrument.Ghost, id, row.value.gcalConfig, row.value.stepCount, row.value.baselineType)
          ).void

        for
          _ <- insertGcalRow
          _ <- insertInstRow
        yield ()

      override def insertGmosNorth(
        id:  Int,
        row: GmosNorthTableRow
      )(using SuperUserAccess): F[Unit] =

        val insertInstRow =
          session.executeCommand(
            Statements.InsertGmosNorth(
              Instrument.GmosNorth                         ,
              id                                           ,
              row.line                                     ,
              row.key.gratingConfig.map(_.grating)         ,
              row.key.filter                               ,
              row.key.fpu                                  ,
              row.key.xBin.value                           ,
              row.key.yBin.value                           ,
              row.key.gratingConfig.map(_.wavelengthRange) ,
              row.key.gratingConfig.map(_.order)           ,
              row.key.gain                                 ,
              row.value.instrumentConfig.exposureTime
            )
          ).void

        val insertGcalRow  =
          session.executeCommand(
            Statements.InsertGcal(Instrument.GmosNorth, id, row.value.gcalConfig, row.value.stepCount, row.value.baselineType)
          ).void

        for {
          _ <- insertGcalRow
          _ <- insertInstRow
        } yield ()

      override def insertGmosSouth(
        id:  Int,
        row: GmosSouthTableRow
      )(using SuperUserAccess): F[Unit] =

        val insertInstRow =
          session.executeCommand(
            Statements.InsertGmosSouth(
              Instrument.GmosSouth                         ,
              id                                           ,
              row.line                                     ,
              row.key.gratingConfig.map(_.grating)         ,
              row.key.filter                               ,
              row.key.fpu                                  ,
              row.key.xBin.value                           ,
              row.key.yBin.value                           ,
              row.key.gratingConfig.map(_.wavelengthRange) ,
              row.key.gratingConfig.map(_.order)           ,
              row.key.gain                                 ,
              row.value.instrumentConfig.exposureTime
            )
          ).void

        val insertGcalRow  =
          session.executeCommand(
            Statements.InsertGcal(Instrument.GmosSouth, id, row.value.gcalConfig, row.value.stepCount, row.value.baselineType)
          ).void

        for {
          _ <- insertGcalRow
          _ <- insertInstRow
        } yield ()

      def insertFlamingos2(
        id:  Int,
        row: Flamingos2TableRow
      )(using SuperUserAccess): F[Unit] =
        val insertGcalRow  =
          session.executeCommand(
            Statements.InsertGcal(
              Instrument.Flamingos2,
              id,
              row.value.gcalConfig,
              row.value.stepCount,
              row.value.baselineType
            )
          ).void

        val insertInstRow =
          session.executeCommand(
            Statements.InsertFlamingos2(
              Instrument.Flamingos2                  ,
              id                                     ,
              row.line                               ,
              row.key.disperser                      ,
              row.key.filter                         ,
              row.key.fpu                            ,
              row.value.instrumentConfig.exposureTime
            )
          ).void

        for {
          _ <- insertGcalRow
          _ <- insertInstRow
        } yield ()

      def insertIgrins2(
        id:  Int,
        row: Igrins2TableRow
      )(using SuperUserAccess): F[Unit] =
        val insertGcalRow =
          session.executeCommand(
            Statements.InsertGcal(
              Instrument.Igrins2,
              id,
              row.value.gcalConfig,
              row.value.stepCount,
              row.value.baselineType
            )
          ).void

        val insertInstRow =
          session.executeCommand(
            Statements.InsertIgrins2(
              Instrument.Igrins2                     ,
              id                                     ,
              row.line                               ,
              row.value.instrumentConfig.exposureTime
            )
          ).void

        for {
          _ <- insertGcalRow
          _ <- insertInstRow
        } yield ()

  object Statements:

    private def whereSmartGcalType(sgt: SmartGcalType): AppliedFragment =
      sgt match
        case SmartGcalType.Arc           => void"g.c_gcal_lamp_type = 'Arc'"
        case SmartGcalType.Flat          => void"g.c_gcal_lamp_type = 'Flat'"
        case SmartGcalType.DayBaseline   => void"g.c_gcal_baseline  = 'Day'"
        case SmartGcalType.NightBaseline => void"g.c_gcal_baseline  = 'Night'"

    private val GcalColumns: List[String] =
      List(
        "c_gcal_continuum",
        "c_gcal_ar_arc",
        "c_gcal_cuar_arc",
        "c_gcal_thar_arc",
        "c_gcal_xe_arc",
        "c_gcal_filter",
        "c_gcal_diffuser",
        "c_gcal_shutter",
        "c_gcal_step_count"
      )

    def gcalColumns(prefix: String): String =
      GcalColumns.map(c => s"$prefix.$c").mkString(",\n")

    private def selectGcal(tableName: String, where: List[AppliedFragment]): AppliedFragment =
      sql"""
        SELECT #${gcalColumns("g")},
               s.c_exposure_time
          FROM #$tableName s
          JOIN t_gcal      g ON s.c_instrument = g.c_instrument
                            AND s.c_gcal_id    = g.c_gcal_id
         WHERE """(Void) |+| where.intercalate(void" AND ") |+| void" ORDER BY s.c_step_order"

    val ghost_config: Codec[GhostUpdate] =
      (
        time_span *:
        int4_pos  *:
        time_span *:
        int4_pos  *:
        time_span
      ).to[GhostUpdate]

    def selectGhost(sgt: SmartGcalType): Query[GhostSearchKey, (Gcal, PosInt, GhostUpdate)] =
      sql"""
        SELECT
          #${gcalColumns("g")},
          s.c_red_exposure_time,
          s.c_red_exposure_count,
          s.c_blue_exposure_time,
          s.c_blue_exposure_count,
          s.c_slit_viewing_camera_exposure_time
        FROM t_smart_ghost s
        JOIN t_gcal g ON s.c_instrument = g.c_instrument
                     AND s.c_gcal_id    = g.c_gcal_id
        WHERE s.c_resolution_mode = $ghost_resolution_mode
          AND s.c_red_binning     = $ghost_binning
          AND s.c_blue_binning    = $ghost_binning
          AND #${whereSmartGcalType(sgt).fragment.sql}
        ORDER BY s.c_step_order
      """.query(step_config_gcal *: int4_pos *: ghost_config).contramap: k =>
        (k.resolutionMode, k.redBinning, k.blueBinning)


    def selectGmosNorth(
      gn:  GmosNorthSearchKey,
      sgt: SmartGcalType
    ): AppliedFragment =
      val where = List(
        sql"s.c_disperser       IS NOT DISTINCT FROM ${gmos_north_grating.opt}"(gn.grating.map(_.grating)),
        sql"s.c_filter          IS NOT DISTINCT FROM ${gmos_north_filter.opt}"(gn.filter),
        sql"s.c_fpu             IS NOT DISTINCT FROM ${gmos_north_fpu.opt}"(gn.fpu),
        sql"s.c_disperser_order IS NOT DISTINCT FROM ${gmos_grating_order.opt}"(gn.grating.map(_.order)),
        sql"s.c_x_binning = ${gmos_binning}"(gn.xBin.value),
        sql"s.c_y_binning = ${gmos_binning}"(gn.yBin.value),
        sql"s.c_amp_gain  = ${gmos_amp_gain}"(gn.gain),
        gn.grating.map(_.wavelength).fold(void"s.c_wavelength_range IS NULL")(
          sql"s.c_wavelength_range @> ${wavelength_pm}"
        ),
        whereSmartGcalType(sgt),
      )

      selectGcal("t_smart_gmos_north", where)

    def selectGmosSouth(
      gs:  GmosSouthSearchKey,
      sgt: SmartGcalType
    ): AppliedFragment =
      val where = List(
        sql"s.c_disperser       IS NOT DISTINCT FROM ${gmos_south_grating.opt}"(gs.grating.map(_.grating)),
        sql"s.c_filter          IS NOT DISTINCT FROM ${gmos_south_filter.opt}"(gs.filter),
        sql"s.c_fpu             IS NOT DISTINCT FROM ${gmos_south_fpu.opt}"(gs.fpu),
        sql"s.c_disperser_order IS NOT DISTINCT FROM ${gmos_grating_order.opt}"(gs.grating.map(_.order)),
        sql"s.c_x_binning = ${gmos_binning}"(gs.xBin.value),
        sql"s.c_y_binning = ${gmos_binning}"(gs.yBin.value),
        sql"s.c_amp_gain  = ${gmos_amp_gain}"(gs.gain),
        gs.grating.map(_.wavelength).fold(void"s.c_wavelength_range IS NULL")(
          sql"s.c_wavelength_range @> ${wavelength_pm}"
        ),
        whereSmartGcalType(sgt),
      )

      selectGcal("t_smart_gmos_south", where)

    def selectF2(
      f2:  Flamingos2SearchKey,
      sgt: SmartGcalType
    ): AppliedFragment =
      val where = List(
        sql"s.c_disperser       IS NOT DISTINCT FROM ${flamingos_2_disperser.opt}"(f2.disperser),
        sql"s.c_filter          IS NOT DISTINCT FROM $flamingos_2_filter"(f2.filter),
        sql"s.c_fpu             IS NOT DISTINCT FROM ${flamingos_2_fpu.opt}"(f2.fpu),
        whereSmartGcalType(sgt),
      )

      selectGcal("t_smart_flamingos2", where)

    val InsertGcal: Fragment[(
      Instrument ,
        Int      ,
        Gcal     ,
        PosInt   ,
        GcalBaselineType
    )] =
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
          $int4_pos,
          $gcal_baseline
      """

    val InsertGhost: Command[(Int, PosLong, GhostTableRow)] =
      sql"""
        INSERT INTO t_smart_ghost (
          c_instrument,
          c_gcal_id,
          c_step_order,
          c_resolution_mode,
          c_red_binning,
          c_blue_binning,
          c_red_exposure_time,
          c_blue_exposure_time,
          c_red_exposure_count,
          c_blue_exposure_count,
          c_slit_viewing_camera_exposure_time
        ) SELECT
          $instrument,
          $int4,
          $int8_pos,
          $ghost_resolution_mode,
          $ghost_binning,
          $ghost_binning,
          $time_span,
          $time_span,
          $int4_pos,
          $int4_pos,
          $time_span
      """.command.contramap { case (gcalId, stepOrder, row) =>
        (
          Instrument.Ghost,
          gcalId,
          stepOrder,
          row.key.resolutionMode,
          row.key.redBinning,
          row.key.blueBinning,
          row.value.instrumentConfig.redExposureTime,
          row.value.instrumentConfig.blueExposureTime,
          row.value.instrumentConfig.redExposureCount,
          row.value.instrumentConfig.blueExposureCount,
          row.value.instrumentConfig.slitExposureTime
        )
      }

    val InsertGmosNorth: Fragment[(
      Instrument                          ,
      Int                                 ,
      PosLong                             ,
      Option[GmosNorthGrating]            ,
      Option[GmosNorthFilter]             ,
      Option[GmosNorthFpu]                ,
      GmosBinning                         ,
      GmosBinning                         ,
      Option[BoundedInterval[Wavelength]] ,
      Option[GmosGratingOrder]            ,
      GmosAmpGain                         ,
      TimeSpan
    )] =
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
          $int8_pos,
          ${gmos_north_grating.opt},
          ${gmos_north_filter.opt},
          ${gmos_north_fpu.opt},
          $gmos_binning,
          $gmos_binning,
          ${wavelength_pm_range.opt},
          ${gmos_grating_order.opt},
          $gmos_amp_gain,
          $time_span
      """

    val InsertGmosSouth: Fragment[(
      Instrument                          ,
      Int                                 ,
      PosLong                             ,
      Option[GmosSouthGrating]            ,
      Option[GmosSouthFilter]             ,
      Option[GmosSouthFpu]                ,
      GmosBinning                         ,
      GmosBinning                         ,
      Option[BoundedInterval[Wavelength]] ,
      Option[GmosGratingOrder]            ,
      GmosAmpGain                         ,
      TimeSpan
    )] =
      sql"""
        INSERT INTO t_smart_gmos_south (
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
          $int8_pos,
          ${gmos_south_grating.opt},
          ${gmos_south_filter.opt},
          ${gmos_south_fpu.opt},
          $gmos_binning,
          $gmos_binning,
          ${wavelength_pm_range.opt},
          ${gmos_grating_order.opt},
          $gmos_amp_gain,
          $time_span
      """

    val InsertFlamingos2: Fragment[(
      Instrument         ,
      Int                ,
      PosLong            ,
      Option[Flamingos2Disperser],
      Flamingos2Filter           ,
      Option[Flamingos2Fpu]      ,
      TimeSpan
    )] =
      sql"""
        INSERT INTO t_smart_flamingos2 (
          c_instrument,
          c_gcal_id,
          c_step_order,
          c_disperser,
          c_filter,
          c_fpu,
          c_exposure_time
        ) SELECT
          $instrument,
          $int4,
          $int8_pos,
          ${flamingos_2_disperser.opt},
          $flamingos_2_filter,
          ${flamingos_2_fpu.opt},
          $time_span
      """

    def selectIgrins2(
      key: Igrins2SearchKey.type,
      sgt: SmartGcalType
    ): AppliedFragment = {
      val where = List(
        whereSmartGcalType(sgt),
      )

      selectGcal("t_smart_igrins2", where)
    }

    val InsertIgrins2: Fragment[(
      Instrument ,
      Int        ,
      PosLong    ,
      TimeSpan
    )] =
      sql"""
        INSERT INTO t_smart_igrins2 (
          c_instrument,
          c_gcal_id,
          c_step_order,
          c_exposure_time
        ) SELECT
          $instrument,
          $int4,
          $int8_pos,
          $time_span
      """