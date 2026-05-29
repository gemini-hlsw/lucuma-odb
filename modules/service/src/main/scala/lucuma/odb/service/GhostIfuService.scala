// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Order.catsKernelOrderingForOrder
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.functorFilter.*
import cats.syntax.option.*
import grackle.Result
import grackle.ResultT
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostReadMode
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.model.sequence.ghost.GhostIfuMapping
import lucuma.core.model.sequence.ghost.GhostStaticConfig
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.odb.data.ExposureTimeModeId
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.input.GhostIfuInput
import lucuma.odb.sequence.ghost.DetectorConfig
import lucuma.odb.sequence.ghost.ifu.Config
import lucuma.odb.sequence.ghost.ifu.GhostIfuMappingSyntax.*
import lucuma.odb.sequence.ghost.ifu.IfuMappingContext
import lucuma.odb.syntax.exposureTimeMode.*
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GhostCodecs.*
import skunk.*
import skunk.codec.temporal.timestamptz
import skunk.data.Completion
import skunk.implicits.*

import Services.Syntax.*

trait GhostIfuService[F[_]]:
  def select(
    which: List[Observation.Id]
  ): F[Map[Observation.Id, Config]]

  def insert(
    input:  GhostIfuInput.Create,
    reqEtm: Option[ExposureTimeMode],
    which:  List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def delete(
    which: List[Observation.Id]
  )(using Transaction[F]): F[Unit]

  def update(
    SET:   GhostIfuInput.Edit,
    which: List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def clone(
    originalId: Observation.Id,
    newId:      Observation.Id,
    etms:       List[(ExposureTimeModeId, ExposureTimeModeId)]
  )(using Transaction[F]): F[Unit]

  def computeStatic(
    observationId: Observation.Id
  ): F[Either[OdbError, GhostStaticConfig]]

  /**
   * Returns a map of the observations for which we cannot derive a valid
   * IFU mapping.  If an observation is not found or has no problems, then
   * it is not included in the results.
   */
  def validationErrors(
    observationIds: List[Observation.Id]
  ): F[Map[Observation.Id, String]]

object GhostIfuService:
  enum Channel:
    case Red, Blue

    def color: String =
      this match
        case Red  => "red"
        case Blue => "blue"

    def etmIdColumn: String =
      s"c_${color}_exposure_time_mode_id"

  private object validateEtms:
    def errorMessage(oids: List[Observation.Id]): String =
      val oidsString = oids match
        case Nil => ""
        case _   => s"${oids.sorted.mkString("(", ", ", ")")} "
      s"GHOST observations ${oidsString}require red and blue channel TimeAndCount exposure time modes with equivalent wavelength values."

    val error: OdbError = OdbError.InvalidArgument(errorMessage(Nil).some)

    def checkAtEquals(a: ExposureTimeMode, b: ExposureTimeMode): Result[Unit] =
      error.asFailure.unlessA(a.at === b.at)

    def forCreate(
      requirementsEtm: Option[ExposureTimeMode],
      scienceEtmA:     Option[ExposureTimeMode],
      scienceEtmB:     Option[ExposureTimeMode]
    ): Result[Unit] =
      def resolveAndValidate(e: Option[ExposureTimeMode]): Result[ExposureTimeMode] =
        Result.fromOption(
          e.orElse(requirementsEtm).flatMap(ExposureTimeMode.timeAndCount.getOption),
          error.asProblem
        )

      for
        a <- resolveAndValidate(scienceEtmA)
        b <- resolveAndValidate(scienceEtmB)
        _ <- checkAtEquals(a, b)
      yield ()

    def forUpdate(a: ExposureTimeMode, b: ExposureTimeMode): Result[Unit] =
      forCreate(none, a.some, b.some)

  def instantiate[F[_]: {Concurrent, Services}]: GhostIfuService[F] =

    new GhostIfuService[F]:
      override def select(
        which:  List[Observation.Id]
      ): F[Map[Observation.Id, Config]] =
        NonEmptyList
          .fromList(which)
          .fold(List.empty.pure[F]): oids =>
            val af = Statements.select(oids)
            session.prepareR(af.fragment.query(observation_id *: Statements.ghost_ifu)).use: pq =>
              pq.stream(af.argument, chunkSize = 1024).compile.toList
          .map(_.toMap)

      override def insert(
        input:  GhostIfuInput.Create,
        reqEtm: Option[ExposureTimeMode],
        which:  List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        val redEtm  = input.red.flatMap(_.exposureTimeMode)
        val blueEtm = input.blue.flatMap(_.exposureTimeMode)
        val science = NonEmptyList.of(Channel.Red -> redEtm, Channel.Blue -> blueEtm)

        NonEmptyList
          .fromList(which)
          .fold(ResultT.unit[F]): nel =>
            for
              _   <- ResultT.fromResult(validateEtms.forCreate(reqEtm, redEtm, blueEtm))
              r   <- ResultT(exposureTimeModeService.resolve("GHOST IFU", none, science, reqEtm, which))
              ids <- ResultT.liftF(exposureTimeModeService.insertResolvedAcquisitionAndScience(r))
              etms = nel.map: oid =>
                val m = ids(oid)._2.toList.toMap
                (oid, m(Channel.Red), m(Channel.Blue))
              _   <- ResultT.liftF(session.exec(Statements.insert(input, etms)))
            yield ()
          .value

      override def delete(
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =
        Statements.delete(which).traverse_(session.exec)

      override def update(
        SET:   GhostIfuInput.Edit,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        def selectEtms(c: Channel): F[List[(Observation.Id, ExposureTimeMode)]] =
          val af = Statements.selectGhostScienceExposureTimeModes(which, c)
          session.prepareR(af.fragment.query(observation_id *: exposure_time_mode)).use: pq =>
            pq.stream(af.argument, chunkSize = 1024)
              .compile
              .toList

        def validateOne(
          updatedEtm:     ExposureTimeMode,
          lookupExisting: F[List[(Observation.Id, ExposureTimeMode)]]
        ): ResultT[F, Unit] =
          ResultT:
            lookupExisting.map: lst =>
              NonEmptyList
                .fromList:
                  lst.collect:
                    case (oid, existingEtm) if !validateEtms.forUpdate(updatedEtm, existingEtm).hasValue => oid
                .fold(Result.unit): oids =>
                   OdbError.InvalidArgument(validateEtms.errorMessage(oids.toList).some).asFailure

        val setRedEtm  = SET.red.flatMap(_.exposureTimeMode)
        val setBlueEtm = SET.blue.flatMap(_.exposureTimeMode)

        val validateEtmUpdate: ResultT[F, Unit] =
          (setRedEtm, setBlueEtm) match
            case (Some(r), Some(b)) => ResultT.fromResult(validateEtms.forUpdate(r, b))
            case (None,    None)    => ResultT.unit
            case (Some(r), None)    => validateOne(r, selectEtms(Channel.Blue))
            case (None,    Some(b)) => validateOne(b, selectEtms(Channel.Red))

        (for
          _ <- ResultT.liftF(Statements.updateGhostIfu(SET, which).traverse_(session.exec))
          _ <- validateEtmUpdate
          _ <- ResultT.liftF(setRedEtm.traverse_(etm => session.exec(Statements.updateEtm(which, etm, Channel.Red))))
          _ <- ResultT.liftF(setBlueEtm.traverse_(etm => session.exec(Statements.updateEtm(which, etm, Channel.Blue))))
        yield ()).value

      override def clone(
        originalId: Observation.Id,
        newId:      Observation.Id,
        etms:       List[(ExposureTimeModeId, ExposureTimeModeId)]
      )(using Transaction[F]): F[Unit] =
        session
          .executeCommand(Statements.clone(originalId, newId, etms))
          .flatMap:
            case Completion.Insert(1) =>
              ().pure[F]
            case _                    =>
              Concurrent[F].raiseError(new RuntimeException(s"Could not clone Ghost IFU observing mode $originalId, $newId"))

      private def selectStaticContext(
        oids: NonEmptyList[Observation.Id]
      ): F[Map[Observation.Id, (IfuMappingContext, Option[TimeSpan])]] =
        val af = Statements.selectStaticContext(oids)
        session.prepareR(af.fragment.query(observation_id *: Statements.mapping_context *: time_span.opt)).use: pq =>
          pq.stream(af.argument, chunkSize = 1024)
            .compile
            .toList
            .map: lst =>
              lst
                .map: (oid, ctx, svcExposureTime) =>
                  oid -> (ctx, svcExposureTime)
                .toMap

      override def computeStatic(
        observationId: Observation.Id
      ): F[Either[OdbError, GhostStaticConfig]] =
        for
          c <- selectStaticContext(NonEmptyList.one(observationId))
          a <- Services.asSuperUser(asterismService.getAsterism(observationId))
        yield
          Either
            .fromOption(c.get(observationId), OdbError.InvalidArgument(s"Observation '$observationId' not found, or not a GHOST observation.".some))
            .flatMap: (mappingCtx, svcTime) =>
              GhostIfuMapping
                .derive(mappingCtx, a.map(_._2))
                .leftMap(s => OdbError.InvalidArgument(s"Could not compute GHOST IFU mapping: $s".some))
                .map: mapping =>
                  GhostStaticConfig(mappingCtx.resolutionMode, mapping, svcTime)

      override def validationErrors(
        observationIds: List[Observation.Id]
      ): F[Map[Observation.Id, String]] =
        NonEmptyList
          .fromList(observationIds)
          .fold(Map.empty.pure[F]): nel =>
            for
              c <- selectStaticContext(nel)
              a <- Services.asSuperUser(asterismService.getAsterisms(observationIds))
            yield
              c.toList
               .mapFilter { case (oid, (mappingCtx, _)) =>
                 val targets = a.getOrElse(oid, Nil).map(_._2)
                 GhostIfuMapping.validate(mappingCtx, targets).map(error => oid -> error)
               }
               .toMap

  object Statements:

    val ghost_detector: Decoder[DetectorConfig] =
      (
        exposure_time_mode  *:
        ghost_binning       *:
        ghost_binning.opt   *:
        ghost_read_mode     *:
        ghost_read_mode.opt
      ).emap: (etm, defaultBinning, explicitBinning, defaultReadMode, explicitReadMode) =>
        ExposureTimeMode
          .timeAndCount
          .getOption(etm)
          .toRight(s"GHOST only supports time and count exposure time mode.")
          .map: tc =>
            DetectorConfig(tc, defaultBinning, explicitBinning, defaultReadMode, explicitReadMode)

    val ghost_detector_blue: Decoder[DetectorConfig.Blue] =
      (ghost_detector).map(DetectorConfig.Blue(_))

    val ghost_detector_red: Decoder[DetectorConfig.Red] =
      (ghost_detector).map(DetectorConfig.Red(_))

    val ghost_ifu: Decoder[Config] =
      (
        int4_pos                      *:
        ghost_resolution_mode         *:
        ghost_detector_red            *:
        ghost_detector_blue           *:
        coordinates.opt               *:
        time_span.opt                 *:
        ghost_ifu1_fiber_agitator.opt *:
        ghost_ifu2_fiber_agitator.opt
      ).to[Config]

    private def detectorColumns(color: String): List[String] =
      List(
        s"c_${color}_exposure_time_mode_id",
        s"c_${color}_binning_default",
        s"c_${color}_binning",
        s"c_${color}_read_mode_default",
        s"c_${color}_read_mode"
      )

    private val RemainingColumns: List[String] =
      List(
        "c_sky_ra",
        "c_sky_dec",
        "c_slit_viewing_camera_exposure_time",
        "c_ifu1_fiber_agitator",
        "c_ifu2_fiber_agitator"
      )

    private val Columns: List[String] =
      List(
        "c_observation_id",
        "c_step_count",
        "c_resolution_mode"
      )                       ++
      detectorColumns("red")  ++
      detectorColumns("blue") ++
      RemainingColumns

    private val ExposureTimeColumns: List[String] =
      List(
        "c_exposure_time_mode",
        "c_signal_to_noise_at",
        "c_signal_to_noise",
        "c_exposure_time",
        "c_exposure_count"
      )

    extension (cols: List[String])
      def prefixed(prefix: String): List[String] = cols.map(c => s"$prefix.$c")
      def string: String = cols.mkString("", ",\n", "\n")
      def insert(index: Int, s: String*): List[String] = cols.patch(index, s.toList, 0)

    def select(
      which: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      sql"""
        SELECT
          g.c_observation_id,
          g.c_step_count,
          g.c_resolution_mode,
          #${ExposureTimeColumns.prefixed("red").string},
          #${detectorColumns("red").tail.prefixed("g").string},
          #${ExposureTimeColumns.prefixed("blue").string},
          #${detectorColumns("blue").tail.prefixed("g").string},
          #${RemainingColumns.prefixed("g").string}
        FROM
          t_ghost_ifu g
        JOIN t_exposure_time_mode red  ON g.c_red_exposure_time_mode_id  = red.c_exposure_time_mode_id
        JOIN t_exposure_time_mode blue ON g.c_blue_exposure_time_mode_id = blue.c_exposure_time_mode_id
        WHERE
          g.c_observation_id IN ("""(Void)                       |+|
            which.map(sql"$observation_id").intercalate(void",") |+|
          void")"

    def insert(
      input: GhostIfuInput.Create,
      which: NonEmptyList[(Observation.Id, ExposureTimeModeId, ExposureTimeModeId)]
    ): AppliedFragment =
      val modeEntries =
        which.map: (oid, red, blue) =>
          sql"""(
            $observation_id,
            $int4_pos,
            $ghost_resolution_mode,
            $exposure_time_mode_id,
            $ghost_binning,
            ${ghost_binning.opt},
            $ghost_read_mode,
            ${ghost_read_mode.opt},
            $exposure_time_mode_id,
            $ghost_binning,
            ${ghost_binning.opt},
            $ghost_read_mode,
            ${ghost_read_mode.opt},
            ${right_ascension.opt},
            ${declination.opt},
            ${time_span.opt},
            ${ghost_ifu1_fiber_agitator.opt},
            ${ghost_ifu2_fiber_agitator.opt}
          )"""(
            oid,
            input.stepCount,
            input.resolutionMode,
            red,
            GhostBinning.OneByOne,
            input.red.flatMap(_.explicitBinning.toOption),
            GhostReadMode.Medium,
            input.red.flatMap(_.explicitReadMode.toOption),
            blue,
            GhostBinning.OneByOne,
            input.blue.flatMap(_.explicitBinning.toOption),
            GhostReadMode.Slow,
            input.blue.flatMap(_.explicitReadMode.toOption),
            input.skyPosition.map(_.ra),
            input.skyPosition.map(_.dec),
            input.slitCameraExposureTime,
            input.explicitIfu1FiberAgitator,
            input.explicitIfu2FiberAgitator
          )

      sql"""
        INSERT INTO t_ghost_ifu (
          #${Columns.insert(1, "c_program_id").string}
        )
        SELECT
          #${Columns.prefixed("g").insert(1, "o.c_program_id").string}
        FROM (
          VALUES"""(Void) |+| modeEntries.intercalate(void", ") |+| sql"""
        ) AS g (
          #${Columns.string}
        )
        JOIN t_observation o ON o.c_observation_id = g.c_observation_id
      """(Void)

    def delete(
      which: List[Observation.Id]
    ): Option[AppliedFragment] =
      NonEmptyList.fromList(which).map: oids =>
        void"DELETE FROM ONLY t_ghost_ifu WHERE " |+| observationIdIn(oids)

    def clone(
      originalId: Observation.Id,
      newId:      Observation.Id,
      etms:       List[(ExposureTimeModeId, ExposureTimeModeId)]
    ): AppliedFragment =

      val replace = List(
        "c_observation_id",
        "c_red_exposure_time_mode_id",
        "c_blue_exposure_time_mode_id"
      )

      val others  = Columns.insert(1, "c_program_id").filterNot(replace.toSet)
      val all     = replace ++ others

      val mappingValues: AppliedFragment =
        etms
          .map: (oldEtm, newEtm) =>
            sql"($exposure_time_mode_id, $exposure_time_mode_id)"(oldEtm, newEtm)
          .intercalate(void", ")

      sql"""
        INSERT INTO t_ghost_ifu (
          #${all.string}
        )
        SELECT
          $observation_id,
          red_map.new_id,
          blue_map.new_id,
          #${others.prefixed("src").string}
        FROM t_ghost_ifu src
        JOIN (VALUES"""(newId) |+| mappingValues |+| void""") AS red_map(old_id, new_id)
          ON red_map.old_id = src.c_red_exposure_time_mode_id
        JOIN (VALUES""" |+| mappingValues |+| sql""") AS blue_map(old_id, new_id)
          ON blue_map.old_id = src.c_blue_exposure_time_mode_id
        WHERE src.c_observation_id = $observation_id"""(originalId)

    def updateEtm(
      oids:    List[Observation.Id],
      update:  ExposureTimeMode,
      channel: Channel
    ): AppliedFragment =
      sql"""
        UPDATE t_exposure_time_mode e
        SET c_exposure_time_mode = $exposure_time_mode_type,
            c_signal_to_noise    = ${signal_to_noise.opt},
            c_signal_to_noise_at = $wavelength_pm,
            c_exposure_time      = ${time_span.opt},
            c_exposure_count     = ${int4_pos.opt}
        FROM t_ghost_ifu g
        WHERE g.c_observation_id        = e.c_observation_id
          AND g.#${channel.etmIdColumn} = e.c_exposure_time_mode_id
          AND e.c_observation_id IN ${observation_id.list(oids.length).values}
      """.apply(
        update.modeType,
        update.signalToNoise,
        update.at,
        update.exposureTime,
        update.exposureCount,
        oids
      )

    def selectGhostScienceExposureTimeModes(
      oids:    List[Observation.Id],
      channel: Channel
    ): AppliedFragment =
      sql"""
        SELECT
          g.c_observation_id,
          e.c_exposure_time_mode,
          e.c_signal_to_noise_at,
          e.c_signal_to_noise,
          e.c_exposure_time,
          e.c_exposure_count
        FROM t_ghost_ifu g
        JOIN t_exposure_time_mode e ON e.c_exposure_time_mode_id = g.#${channel.etmIdColumn}
        WHERE g.c_observation_id IN ${observation_id.list(oids.length).values}
      """.apply(oids.toList)

    def updateGhostIfu(
      SET:   GhostIfuInput.Edit,
      which: List[Observation.Id]
    ): Option[AppliedFragment] =
      val updates =
        val upStepCount      = sql"c_step_count          = $int4_pos"
        val upResolutionMode = sql"c_resolution_mode     = $ghost_resolution_mode"
        val upRedBinning     = sql"c_red_binning         = ${ghost_binning.opt}"
        val upRedReadMode    = sql"c_red_read_mode       = ${ghost_read_mode.opt}"
        val upBlueBinning    = sql"c_blue_binning        = ${ghost_binning.opt}"
        val upBlueReadMode   = sql"c_blue_read_mode      = ${ghost_read_mode.opt}"
        val upSkyRa          = sql"c_sky_ra              = ${right_ascension.opt}"
        val upSkyDec         = sql"c_sky_dec             = ${declination.opt}"
        val upSlitExpTime    = sql"c_slit_viewing_camera_exposure_time = ${time_span.opt}"
        val upIfu1Agitator   = sql"c_ifu1_fiber_agitator = ${ghost_ifu1_fiber_agitator.opt}"
        val upIfu2Agitator   = sql"c_ifu2_fiber_agitator = ${ghost_ifu2_fiber_agitator.opt}"

        NonEmptyList.fromList:
          List(
            SET.stepCount.map(upStepCount),
            SET.resolutionMode.map(upResolutionMode),
            SET.red.flatMap(_.explicitBinning.toOptionOption).map(upRedBinning),
            SET.red.flatMap(_.explicitReadMode.toOptionOption).map(upRedReadMode),
            SET.blue.flatMap(_.explicitBinning.toOptionOption).map(upBlueBinning),
            SET.blue.flatMap(_.explicitReadMode.toOptionOption).map(upBlueReadMode),
            SET.skyPosition.toOptionOption.map(_.flatMap(_.ra)).map(upSkyRa),
            SET.skyPosition.toOptionOption.map(_.flatMap(_.dec)).map(upSkyDec),
            SET.slitCameraExposureTime.toOptionOption.map(upSlitExpTime),
            SET.explicitIfu1FiberAgitator.toOptionOption.map(upIfu1Agitator),
            SET.explicitIfu2FiberAgitator.toOptionOption.map(upIfu2Agitator)
          ).flatten

      for
        us <- updates
        os <- NonEmptyList.fromList(which)
      yield
        void"UPDATE t_ghost_ifu " |+|
          void"SET " |+| us.intercalate(void", ") |+| void" " |+|
          void"WHERE " |+| observationIdIn(os)

    val mapping_context: Decoder[IfuMappingContext] =
      (
        ghost_resolution_mode *:
        coordinates.opt       *:
        timestamptz(6)        *:
        core_timestamp.opt    *:
        pos_angle_constraint  *:
        coordinates.opt
      ).map: (resMode, sky, now, obsTime, pac, explicitBase) =>
        val t = obsTime.getOrElse(Timestamp.unsafeFromInstantTruncated(now.toInstant))
        IfuMappingContext(resMode, sky, pac, explicitBase, t)

    def selectStaticContext(
      oids: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      sql"""
        SELECT
          o.c_observation_id,
          g.c_resolution_mode,
          g.c_sky_ra,
          g.c_sky_dec,
          CURRENT_TIMESTAMP(6),
          o.c_observation_time,
          o.c_pac_mode,
          o.c_pac_angle,
          o.c_explicit_ra,
          o.c_explicit_dec,
          g.c_slit_viewing_camera_exposure_time
        FROM
          t_ghost_ifu g
        INNER JOIN
          t_observation o ON o.c_observation_id = g.c_observation_id
        WHERE
          g.c_observation_id IN ${observation_id.list(oids.length).values}
      """.apply(oids.toList)