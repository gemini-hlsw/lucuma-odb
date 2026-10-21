// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.implicits.*
import eu.timepit.refined.types.numeric.PosInt
import grackle.Result
import grackle.ResultT
import lucuma.core.enums.CalibrationSetType
import lucuma.core.enums.GcalArc
import lucuma.core.enums.Instrument
import lucuma.core.model.Observation
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.input.CalibrationSetInput
import lucuma.odb.graphql.input.DaytimeCalibrationInput
import lucuma.odb.sequence.daytimecal.CalibrationSet
import lucuma.odb.sequence.daytimecal.Config
import lucuma.odb.sequence.daytimecal.InstrumentSetup
import lucuma.odb.service.Services.Syntax.session
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import skunk.*
import skunk.codec.boolean.bool
import skunk.codec.numeric.int4
import skunk.codec.numeric.int8
import skunk.data.Completion
import skunk.syntax.all.*

trait DaytimeCalibrationService[F[_]]:

  def select(
    which: List[Observation.Id]
  ): F[Map[Observation.Id, Config]]

  def insert(
    input: DaytimeCalibrationInput.Create,
    which: List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def delete(
    which: List[Observation.Id]
  )(using Transaction[F]): F[Unit]

  def update(
    SET: DaytimeCalibrationInput.Edit,
    which: List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def clone(
    originalId: Observation.Id,
    newId: Observation.Id
  )(using Transaction[F]): F[Unit]

object DaytimeCalibrationService:

  def instantiate[F[_]: Concurrent](using Services[F]): DaytimeCalibrationService[F] =
    new DaytimeCalibrationService[F]:

      override def select(
        which: List[Observation.Id]
      ): F[Map[Observation.Id, Config]] =
        NonEmptyList.fromList(which) match
          case None      => Map.empty.pure
          case Some(nel) =>
            for
              insts <- session.prepareR(Statements.selectInstrument(nel)).use(_.stream(nel, 1024).compile.toList)
              sets  <- session.prepareR(Statements.selectSets(nel)).use(_.stream(nel, 1024).compile.toList)
            yield
              val byObs = sets.groupMap(_._1)(_._2)
              insts.map((oid, inst) => oid -> Config(inst, byObs.getOrElse(oid, Nil))).toMap

      private def insertSets(
        oid:  Observation.Id,
        sets: List[CalibrationSet[InstrumentSetup]]
      ): F[Unit] =
        sets.zipWithIndex.traverse_ : (set, position) =>
          session.unique(Statements.InsertSet)((oid, position, set)).flatMap: setId =>
            set.setup match
              case InstrumentSetup.GmosNorth(c) => session.execute(Statements.InsertGmosNorthSetup)((setId, oid, c)).void
              case InstrumentSetup.GmosSouth(c) => session.execute(Statements.InsertGmosSouthSetup)((setId, oid, c)).void

      override def insert(
        input: DaytimeCalibrationInput.Create,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        session.prepareR(Statements.Insert).use: ps =>
          which
            .traverse: oid =>
              ps.execute((oid, input.instrument)).flatMap:
                case Completion.Insert(1) => insertSets(oid, input.sets).as(Result.unit)
                case other                => Result.failure(s"Expected single insert, got $other.").pure
            .map(_.sequenceVoid)

      override def delete(
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =
        NonEmptyList.fromList(which) match
          case None      => ().pure
          case Some(nel) =>
            session.prepareR(Statements.delete(nel)).use: ps =>
              ps.execute(nel).void

      // Rows drive the sequence only until it is materialized; afterwards
      // they are frozen.
      private def checkNotMaterialized(oids: NonEmptyList[Observation.Id]): F[Result[Unit]] =
        session.execute(Statements.selectWithAtoms(oids))(oids).map: withAtoms =>
          if withAtoms.isEmpty then Result.unit
          else OdbError.InvalidArgument(
            s"Cannot change the calibration sets of a daytime calibration with a stored sequence (${withAtoms.mkString(", ")}). Delete the sequence first.".some
          ).asFailure

      override def update(
        SET: DaytimeCalibrationInput.Edit,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        NonEmptyList.fromList(which).fold(Result.unit.pure): oids =>
          select(which).flatMap: existing =>
            val checks: Result[Unit] =
              existing.toList.traverse_ : (_, config) =>
                val inst = SET.instrument.getOrElse(config.instrument)
                val sets = SET.sets.getOrElse(config.sets)
                CalibrationSetInput.instrumentMismatch(inst, sets).fold(Result.unit): m =>
                  OdbError.InvalidArgument(m.some).asFailure

            val materialized: F[Result[Unit]] =
              if SET.sets.isDefined then checkNotMaterialized(oids) else Result.unit.pure

            val updateInstrument: F[Unit] =
              SET.instrument.traverse_ : inst =>
                val af = Statements.updateInstrument(inst, oids)
                session.prepareR(af.fragment.command).use(_.execute(af.argument)).void

            val replaceSets: F[Unit] =
              SET.sets.traverse_ : sets =>
                session.prepareR(Statements.deleteSets(oids)).use(_.execute(oids)) *>
                  which.traverse_(oid => insertSets(oid, sets))

            (for
              _ <- ResultT(checks.pure)
              _ <- ResultT(materialized)
              _ <- ResultT.liftF(updateInstrument *> replaceSets)
            yield ()).value

      override def clone(
        originalId: Observation.Id,
        newId: Observation.Id
      )(using Transaction[F]): F[Unit] =
        select(List(originalId)).map(_.get(originalId)).flatMap:
          case None         =>
            Concurrent[F].raiseError(new RuntimeException(s"Clone: original observation $originalId has no daytime calibration config."))
          case Some(config) =>
            insert(config, List(newId)).flatMap: res =>
              res.toEither match
                case Left(Left(t))   => Concurrent[F].raiseError(t)
                case Left(Right(ps)) => Concurrent[F].raiseError(new RuntimeException(s"Clone: insert failed: ${ps.toList.mkString(", ")}"))
                case Right(a)        => a.pure

  private object Statements:

    val pos_int: Codec[PosInt] =
      int4.eimap(i => PosInt.from(i))(_.value)

    // GCAL columns, all null for kinds that take no lamp.
    val gcal_opt: Codec[Option[StepConfig.Gcal]] =
      (gcal_continuum.opt *:
       bool               *:
       bool               *:
       bool               *:
       bool               *:
       gcal_filter.opt    *:
       gcal_diffuser.opt  *:
       gcal_shutter.opt
      ).eimap { case (c, ar, cuar, thar, xe, f, d, s) =>
        (f, d, s) match
          case (Some(f), Some(d), Some(s)) =>
            val arcs = List(ar -> GcalArc.ArArc, cuar -> GcalArc.CuArArc, thar -> GcalArc.ThArArc, xe -> GcalArc.XeArc).collect { case (true, a) => a }
            StepConfig.Gcal.Lamp.fromContinuumOrArcs(c, arcs).map(l => StepConfig.Gcal(l, f, d, s).some)
          case _                           => 
            none.asRight
      } {
        case None    => 
          (none, false, false, false, false, none, none, none)
        case Some(g) =>
          val arcs = g.lamp.toArcsSortedSet
          (g.lamp.continuum, arcs(GcalArc.ArArc), arcs(GcalArc.CuArArc), arcs(GcalArc.ThArArc), arcs(GcalArc.XeArc), g.filter.some, g.diffuser.some, g.shutter.some)
      }

    val calibration_set: Decoder[CalibrationSet[InstrumentSetup]] =
      (calibration_set_type    *:
       pos_int                 *:
       gcal_opt                *:
       gmos_north_dynamic.opt  *:
       gmos_south_dynamic.opt
      ).emap:
        case (k, n, g, Some(north), None) => CalibrationSet(k, n, g, InstrumentSetup.GmosNorth(north)).asRight
        case (k, n, g, None, Some(south)) => CalibrationSet(k, n, g, InstrumentSetup.GmosSouth(south)).asRight
        case _                            => "Calibration set without exactly one instrument setup".asLeft

    def selectInstrument[A <: NonEmptyList[Observation.Id]](a: A): Query[a.type, (Observation.Id, Instrument)] =
      sql"""
        SELECT
          c_observation_id,
          c_instrument
        FROM
          t_daytime_calibration
        WHERE
          c_observation_id IN (${observation_id.nel(a)})
      """.query(observation_id *: instrument)

    def selectSets[A <: NonEmptyList[Observation.Id]](a: A): Query[a.type, (Observation.Id, CalibrationSet[InstrumentSetup])] =
      sql"""
        SELECT
          s.c_observation_id,
          s.c_kind,
          s.c_count,
          s.c_gcal_continuum,
          s.c_gcal_ar_arc,
          s.c_gcal_cuar_arc,
          s.c_gcal_thar_arc,
          s.c_gcal_xe_arc,
          s.c_gcal_filter,
          s.c_gcal_diffuser,
          s.c_gcal_shutter,
          #${encodeColumns("n".some, GmosSequenceService.Statements.GmosDynamicColumns)},
          #${encodeColumns("so".some, GmosSequenceService.Statements.GmosDynamicColumns)}
        FROM t_daytime_calibration_set s
        LEFT JOIN t_gmos_north_daytime_calibration_setup n ON n.c_set_id = s.c_set_id
        LEFT JOIN t_gmos_south_daytime_calibration_setup so ON so.c_set_id = s.c_set_id
        WHERE
          s.c_observation_id IN (${observation_id.nel(a)})
        ORDER BY s.c_observation_id, s.c_position
      """.query(observation_id *: calibration_set)

    val Insert: Command[(Observation.Id, Instrument)] =
      sql"""
        INSERT INTO t_daytime_calibration (
          c_observation_id,
          c_instrument
        ) VALUES (
          $observation_id,
          $instrument
        )
      """.command

    val InsertSet: Query[(Observation.Id, Int, CalibrationSet[InstrumentSetup]), Long] =
      sql"""
        INSERT INTO t_daytime_calibration_set (
          c_observation_id,
          c_position,
          c_kind,
          c_count,
          c_gcal_continuum,
          c_gcal_ar_arc,
          c_gcal_cuar_arc,
          c_gcal_thar_arc,
          c_gcal_xe_arc,
          c_gcal_filter,
          c_gcal_diffuser,
          c_gcal_shutter
        ) VALUES (
          $observation_id,
          $int4,
          $calibration_set_type,
          $pos_int,
          $gcal_opt
        )
        RETURNING c_set_id
      """.query(int8).contramap((oid, pos, set) => (oid, pos, set.kind, set.count, set.gcal))

    private def insertSetup(site: String): Fragment[Void] =
      sql"""
        INSERT INTO t_gmos_#${site}_daytime_calibration_setup (
          c_set_id,
          c_observation_id,
          #${encodeColumns(none, GmosSequenceService.Statements.GmosDynamicColumns)}
        )
      """

    val InsertGmosNorthSetup: Command[(Long, Observation.Id, DynamicConfig.GmosNorth)] =
      (insertSetup("north") ~> sql"SELECT $int8, $observation_id, $gmos_north_dynamic").command

    val InsertGmosSouthSetup: Command[(Long, Observation.Id, DynamicConfig.GmosSouth)] =
      (insertSetup("south") ~> sql"SELECT $int8, $observation_id, $gmos_south_dynamic").command

    def delete[A <: NonEmptyList[Observation.Id]](a: A): Command[a.type] =
      sql"""
        DELETE FROM t_daytime_calibration
        WHERE c_observation_id IN (${observation_id.nel(a)})
      """.command

    def deleteSets[A <: NonEmptyList[Observation.Id]](a: A): Command[a.type] =
      sql"""
        DELETE FROM t_daytime_calibration_set
        WHERE c_observation_id IN (${observation_id.nel(a)})
      """.command

    def updateInstrument(inst: Instrument, oids: NonEmptyList[Observation.Id]): AppliedFragment =
      sql"""
        UPDATE t_daytime_calibration
        SET c_instrument = $instrument
        WHERE c_observation_id IN (${observation_id.nel(oids)})
      """(inst, oids)

    def selectWithAtoms[A <: NonEmptyList[Observation.Id]](a: A): Query[a.type, Observation.Id] =
      sql"""
        SELECT DISTINCT c_observation_id
        FROM t_atom
        WHERE c_observation_id IN (${observation_id.nel(a)})
      """.query(observation_id)
