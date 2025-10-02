// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import grackle.ResultT
import grackle.syntax.*
import lucuma.core.enums.ArcType
import lucuma.core.math.Arc
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.ProperMotion
import lucuma.core.math.Region
import lucuma.core.math.RightAscension
import lucuma.core.model.CatalogInfo
import lucuma.core.model.EphemerisKey
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import lucuma.odb.data.Nullable
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.input.EditAsterismsPatchInput
import lucuma.odb.graphql.mapping.AccessControl
import lucuma.odb.json.all.query.given
import lucuma.odb.service.Services.SuperUserAccess
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.circe.codec.all.*
import skunk.codec.all.*
import skunk.implicits.*

import Services.Syntax.*

trait AsterismService[F[_]] {

  /**
   * Inserts (program, observation, target) triplets covering all combinations.
   * In other words, every observation in `observationIds` will be given all
   * the targets in `targetIds` in addition to any existing targets they may
   * already have.
   */
  def insertAsterism(
    programId:      Program.Id,
    observationIds: NonEmptyList[Observation.Id],
    targetIds:      NonEmptyList[Target.Id]
  )(using Transaction[F], SuperUserAccess): F[Result[Unit]]

  /**
   * Replaces the existing asterisms associated with the given observation ids
   * (if any) with the given targets.  This is essentially a delete followed
   * by an insert.
   */
  def setAsterism(
    programId:      Program.Id,
    observationIds: NonEmptyList[Observation.Id],
    targetIds:      Nullable[NonEmptyList[Target.Id]]
  )(using Transaction[F], SuperUserAccess): F[Result[Unit]]

  /**
   * Updates the asterisms associated with each observation id, adding and
   * deleting targets as indicated.
   */
  def updateAsterism(
    update: AccessControl.CheckedWithIds[EditAsterismsPatchInput, Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def cloneAsterism(
    originalId: Observation.Id,
    newId: Observation.Id,
  )(using Transaction[F], SuperUserAccess): F[Unit]

  def getAsterism(
    programId: Program.Id,
    observationId: Observation.Id
  )(using SuperUserAccess): F[List[(Target.Id, Target)]]

  def getAsterisms(
    oids: List[Observation.Id]
  )(using SuperUserAccess): F[Map[Observation.Id, List[(Target.Id, Target)]]]
}

object AsterismService {

  def instantiate[F[_]: Concurrent](using Services[F]): AsterismService[F] =

    new AsterismService[F] {

      // Selects the program id that is shared by all the given observations,
      // or else fails.
      private def selectProgramId(
        observationIds: NonEmptyList[Observation.Id]
      ): F[Result[Program.Id]] = {
        val af = Statements.selectProgramId(observationIds)
        session.prepareR(af.fragment.query(program_id)).use { p =>
          p.stream(af.argument, chunkSize = 16)
           .take(2)
           .compile
           .toList
           .map {
             case List(pid) => pid.success
             case _         => OdbError.InvalidObservationList(observationIds).asFailure
           }
        }
      }

      private def containsBlindOffset(
        targetIds: NonEmptyList[Target.Id]
      ): F[Boolean] = 
        val enc = target_id.nel(targetIds)
        session.unique(Statements.containsBlindOffset(enc))(targetIds)
      
      private def deleteLinks(
        programId:      Program.Id,
        observationIds: NonEmptyList[Observation.Id],
        targetIds:      NonEmptyList[Target.Id]
      ): F[Result[Unit]] =
        containsBlindOffset(targetIds).flatMap: hasBlindOffset =>
          if (hasBlindOffset)
            OdbError.InvalidArgument("Blind offset targets cannot be removed from an asterism.".some).asFailureF
          else
            val af = Statements.deleteLinks(programId, observationIds, targetIds)
            session.prepareR(af.fragment.command).use { p =>
              p.execute(af.argument).as(Result.unit)
            }

      override def insertAsterism(
        programId:      Program.Id,
        observationIds: NonEmptyList[Observation.Id],
        targetIds:      NonEmptyList[Target.Id]
      )(using Transaction[F], SuperUserAccess): F[Result[Unit]] = {
        containsBlindOffset(targetIds).flatMap: hasBlindOffset =>
          if (hasBlindOffset)
            OdbError.InvalidArgument("Blind offset targets cannot be added to an asterism.".some).asFailureF
          else
            val af = Statements.insertLinks(programId, observationIds, targetIds)
            session.prepareR(af.fragment.command).use: p =>
              p.execute(af.argument)
                .as(Result.unit)
                .recoverWith:
                  case SqlState.ForeignKeyViolation(_) =>
                    OdbError.InvalidTargetList(programId, targetIds).asFailureF
      }

      @annotation.nowarn("msg=unused implicit parameter")
      def deleteAsterism(
        programId:      Program.Id,
        observationIds: NonEmptyList[Observation.Id]
      )(using Transaction[F], SuperUserAccess): F[Result[Unit]] =
        val af = Statements.deleteAllLinks(programId, observationIds)
        session.prepareR(af.fragment.command).use { p =>
          p.execute(af.argument).as(Result.unit)
        }

      override def setAsterism(
        programId:      Program.Id,
        observationIds: NonEmptyList[Observation.Id],
        targetIds:      Nullable[NonEmptyList[Target.Id]]
      )(using Transaction[F], SuperUserAccess): F[Result[Unit]] =
        targetIds match {
          case Nullable.Null          =>
            deleteAsterism(programId, observationIds)

          case Nullable.Absent        =>
            Result.unit.pure[F]

          case Nullable.NonNull(tids) =>
            deleteAsterism(programId, observationIds) *>
              insertAsterism(programId, observationIds, tids)
        }

      override def updateAsterism(
        update: AccessControl.CheckedWithIds[EditAsterismsPatchInput, Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        update.foldWithIds(Result.unit.pure[F]): (set, observationIds) =>
          val ADD = set.ADD.flatMap(NonEmptyList.fromList)
          val DELETE = set.DELETE.flatMap(NonEmptyList.fromList)
          ResultT(selectProgramId(observationIds)).flatMap { pid =>
            ResultT(ADD.fold(Result.unit.pure[F])(Services.asSuperUser(insertAsterism(pid, observationIds, _)))) *>
              ResultT(DELETE.fold(Result.unit.pure[F]) { tids =>
                deleteLinks(pid, observationIds, tids)
              })
          }.value

      override def cloneAsterism(
        originalId: Observation.Id,
        newId: Observation.Id,
      )(using Transaction[F], SuperUserAccess): F[Unit] =
        val clone = Statements.clone(originalId, newId)
        session.prepareR(clone.fragment.command).use { ps =>
          ps.execute(clone.argument).void
        }

      override def getAsterism(
        programId: Program.Id,
        observationId: Observation.Id
      )(using SuperUserAccess): F[List[(Target.Id, Target)]] =
        val af = Statements.getAsterism(programId, observationId)
        session.prepareR(af.fragment.query(Decoders.targetDecoder)).use { ps =>
          ps.stream(af.argument, chunkSize = 1024).compile.toList
        }

      override def getAsterisms(
        oids: List[Observation.Id]
      )(using SuperUserAccess): F[Map[Observation.Id, List[(Target.Id, Target)]]] =
        NonEmptyList.fromList(oids) match
          case None      => Map.empty.pure[F]
          case Some(nel) =>
            val enc = observation_id.nel(nel)
            session
              .stream(Statements.getAsterisms(enc))(nel, 1024)
              .compile
              .toList
              .map(_.groupMap(_._1)(_._2))

    }

  object Statements {

    def selectProgramId(
      observationIds: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      void"""
        SELECT DISTINCT c_program_id
        FROM t_observation
        WHERE """ |+| observationIdIn(observationIds)

    def insertLinks(
      programId:      Program.Id,
      observationIds: NonEmptyList[Observation.Id],
      targetIds:      NonEmptyList[Target.Id]
    ): AppliedFragment = {
      val insert: AppliedFragment =
        void"""
          INSERT INTO t_asterism_target (
            c_program_id,
            c_observation_id,
            c_target_id
          )
          SELECT * FROM (
            VALUES
        """

      val links: NonEmptyList[AppliedFragment] =
        for {
          oid <- observationIds
          tid <- targetIds
        } yield sql"($program_id, $observation_id, $target_id)"(programId, oid, tid)

      val values: AppliedFragment =
        links.intercalate(void", ")

      val as: AppliedFragment =
        void""") AS t (c_program_id, c_observation_id, c_target_id) """

      insert |+| values |+| as |+|
        void""" ON CONFLICT DO NOTHING"""  // the key consists of all the columns anyway
    }

    // assumes table alias of 'a'
    private def programIdEqual(
      programId: Program.Id
    ): AppliedFragment =
      sql"a.c_program_id = $program_id"(programId)

    // assumes table alias of 'a'
    private def targetIdIn(
      targetIds: NonEmptyList[Target.Id]
    ): AppliedFragment =
      void"a.c_target_id IN (" |+|
        targetIds.map(sql"$target_id").intercalate(void", ") |+|
      void")"

    def deleteLinks(
      programId:      Program.Id,
      observationIds: NonEmptyList[Observation.Id],
      targetIds:      NonEmptyList[Target.Id]
    ): AppliedFragment =
      void"DELETE FROM ONLY t_asterism_target a "                        |+|
        void"USING t_target t "                                          |+|
        void"WHERE " |+| programIdEqual(programId)                       |+|
        void" AND " |+| observationIdIn(observationIds)                  |+|
        void" AND " |+| targetIdIn(targetIds)                            |+|
        void" AND a.c_target_id = t.c_target_id "                        |+|
        void" AND t.c_target_disposition != 'blind_offset' "  

    def deleteAllLinks(
      programId:      Program.Id,
      observationIds: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      void"DELETE FROM ONLY t_asterism_target a "                        |+|
        void"USING t_target t "                                          |+|
        void"WHERE " |+| programIdEqual(programId)                       |+|
        void" AND "  |+| observationIdIn(observationIds)                 |+|
        void" AND a.c_target_id = t.c_target_id "                        |+|
        void" AND t.c_target_disposition != 'blind_offset' "  

    // programs that aren't visible.
    def clone(originalOid: Observation.Id, newOid: Observation.Id): AppliedFragment =
      sql"""
        INSERT INTO t_asterism_target (
          c_program_id,
          c_observation_id,
          c_target_id
        )
        SELECT
          t_asterism_target.c_program_id,
          $observation_id,
          t_asterism_target.c_target_id
        FROM t_asterism_target
        JOIN t_target ON t_target.c_target_id = t_asterism_target.c_target_id
        WHERE c_observation_id = $observation_id
        AND t_target.c_existence = 'present' -- don't clone references to deleted targets, but do clone blind offsets
      """.apply(newOid, originalOid)

    def getAsterism(pid: Program.Id, oid: Observation.Id): AppliedFragment =
      sql"""
        select
          t.c_target_id,
          c_name,
          c_sid_ra,
          c_sid_dec,
          c_sid_epoch,
          c_sid_pm_ra,
          c_sid_pm_dec,
          c_sid_rv,
          c_sid_parallax,
          c_sid_catalog_name,
          c_sid_catalog_id,
          c_sid_catalog_object_type,
          c_nsid_des,
          c_nsid_key_type,
          c_opp_ra_arc_type,
          c_opp_ra_arc_start,
          c_opp_ra_arc_end,
          c_opp_dec_arc_type,
          c_opp_dec_arc_start,
          c_opp_dec_arc_end,
          c_source_profile
        from t_target t
        inner join t_asterism_target a
        on t.c_target_id = a.c_target_id
          and a.c_program_id = $program_id
          and a.c_observation_id = $observation_id
        where t.c_existence = 'present'
          and t.c_target_disposition != 'blind_offset'
      """.apply(pid, oid)

    def getAsterisms[A <: NonEmptyList[Observation.Id]](enc: Encoder[A]): Query[A, (Observation.Id, (Target.Id, Target))] =
      sql"""
        select
          a.c_observation_id,
          t.c_target_id,
          c_name,
          c_sid_ra,
          c_sid_dec,
          c_sid_epoch,
          c_sid_pm_ra,
          c_sid_pm_dec,
          c_sid_rv,
          c_sid_parallax,
          c_sid_catalog_name,
          c_sid_catalog_id,
          c_sid_catalog_object_type,
          c_nsid_des,
          c_nsid_key_type,
          c_opp_ra_arc_type,
          c_opp_ra_arc_start,
          c_opp_ra_arc_end,
          c_opp_dec_arc_type,
          c_opp_dec_arc_start,
          c_opp_dec_arc_end,
          c_source_profile
        from t_target t
        inner join t_asterism_target a
        on t.c_target_id = a.c_target_id
          and a.c_observation_id in ($enc)
        where t.c_existence = 'present'
          and t.c_target_disposition != 'blind_offset'
      """.query(observation_id ~ Decoders.targetDecoder)

    def containsBlindOffset[A <: NonEmptyList[Target.Id]](enc: Encoder[A]): Query[A, Boolean] =
      sql"""
        select count(1)
        from t_target
        where c_target_id in ($enc)
          and c_target_disposition = 'blind_offset'
      """.query(int8).map(_ > 0)
  }

  object Decoders {
    def toNonEmpty(s: String): Option[NonEmptyString] = NonEmptyString.from(s).toOption

    val targetDecoder: Decoder[(Target.Id, Target)] =
      (target_id *:
        text_nonempty *:           // name
        right_ascension.opt *:
        declination.opt *:
        epoch.opt *:
        int8.opt *:               // proper motion ra
        int8.opt *:               // proper motion dec
        radial_velocity.opt *:
        parallax.opt *:
        catalog_name.opt *:
        varchar.opt *:            // catalog id
        varchar.opt *:            // catalog object type
        varchar.opt *:            // ns des
        ephemeris_key_type.opt *: // ns ephemeris key type
        arc_type.opt *:
        right_ascension.opt *:
        right_ascension.opt *:
        arc_type.opt *:
        declination.opt *:
        declination.opt *:          
        jsonb                     // source profile
      ).emap {
        case (id,
              name,
              oRa,
              oDec,
              oEpoch,
              oPmRa,
              oPmDec,
              oRv,
              oParallax,
              oCatName,
              oCatId,
              oCatType,
              oDes,
              oEphemKeyType,
              oRaArcType,
              oRaArcStart,
              oRaArcEnd,
              oDecArcType,
              oDecArcStart,
              oDecArcEnd,
              profile
            ) =>
          val oSourceProfile = profile.as[SourceProfile].toOption
          (oRa, oDec, oEpoch, oSourceProfile)
            .mapN { (ra, dec, epoch, sourceProfile) =>
              val baseCoords   = Coordinates(ra, dec)
              val properMotion = (oPmRa, oPmDec).mapN((pmRa, pmDec) =>
                ProperMotion(ProperMotion.μasyRA(pmRa), ProperMotion.μasyDec(pmDec))
              )
              val catalogInfo  = (oCatName, oCatId.flatMap(toNonEmpty)).mapN { (name, id) =>
                CatalogInfo(name, id, oCatType.flatMap(toNonEmpty))
              }
              val tracking     = SiderealTracking(baseCoords, epoch, properMotion, oRv, oParallax)
              (id, Target.Sidereal(name, tracking, sourceProfile, catalogInfo))
            }
            .orElse(
              (oDes, oEphemKeyType, oSourceProfile)
                .mapN((des, keyType, sourceProfile) =>
                  EphemerisKey.fromTypeAndDes
                    .getOption((keyType, des))
                    .map(key => (id, Target.Nonsidereal(name, key, sourceProfile)))
                )
                .flatten
            )
            .orElse(
              (oRaArcType, oDecArcType, oSourceProfile).tupled.flatMap: (raArcType, decArcType, sourceProfile) =>

                val oRaArc: Option[Arc[RightAscension]] =
                  (raArcType, oRaArcStart, oRaArcEnd) match
                    case (ArcType.Empty, None, None) => Arc.Empty[RightAscension]().some
                    case (ArcType.Full, None, None) => Arc.Full[RightAscension]().some
                    case (ArcType.Partial, Some(s), Some(e)) => Arc.Partial(s, e).some
                    case _ => None

                val oDecArc: Option[Arc[Declination]] =
                  (decArcType, oDecArcStart, oDecArcEnd) match
                    case (ArcType.Empty, None, None) => Arc.Empty[Declination]().some
                    case (ArcType.Full, None, None) => Arc.Full[Declination]().some
                    case (ArcType.Partial, Some(s), Some(e)) => Arc.Partial(s, e).some
                    case _ => None
                  
                (oRaArc, oDecArc).mapN: (raArc, decArc) =>
                  (id, Target.Opportunity(name, Region(raArc, decArc), sourceProfile))

            )
            .toRight(s"Invalid target $id.")
      }

  }
}
