// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.apply.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import grackle.ResultT
import grackle.syntax.*
import lucuma.core.math.Coordinates
import lucuma.core.math.ProperMotion
import lucuma.core.model.CatalogInfo
import lucuma.core.model.EphemerisKey
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.data.Nullable
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.input.EditAsterismsPatchInput
import lucuma.odb.graphql.mapping.AccessControl
import lucuma.odb.json.all.query.given
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
  )(using Transaction[F]): F[Result[Unit]]

  /**
   * Deletes the asterisms associated with the given observation ids.
   */
  def deleteAsterism(
    programId:      Program.Id,
    observationIds: NonEmptyList[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  /**
   * Replaces the existing asterisms associated with the given observation ids
   * (if any) with the given targets.  This is essentially a delete followed
   * by an insert.
   */
  def setAsterism(
    programId:      Program.Id,
    observationIds: NonEmptyList[Observation.Id],
    targetIds:      Nullable[NonEmptyList[Target.Id]]
  )(using Transaction[F]): F[Result[Unit]]

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
  )(using Transaction[F]): F[Unit]

  def getAsterism(
    programId: Program.Id,
    observationId: Observation.Id
  ): F[List[(Target.Id, Target)]]

  def getAsterisms(
    oids: List[Observation.Id]
  ): F[Map[Observation.Id, List[(Target.Id, Target)]]]
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

      override def insertAsterism(
        programId:      Program.Id,
        observationIds: NonEmptyList[Observation.Id],
        targetIds:      NonEmptyList[Target.Id]
      )(using Transaction[F]): F[Result[Unit]] = {
        val af = Statements.insertLinksAs(user, programId, observationIds, targetIds)
        session.prepareR(af.fragment.command).use { p =>
          p.execute(af.argument)
            .as(Result.unit)
            .recoverWith {
              case SqlState.ForeignKeyViolation(_) =>
                OdbError.InvalidTargetList(programId, targetIds).asFailureF
            }
        }
      }

      override def deleteAsterism(
        programId:      Program.Id,
        observationIds: NonEmptyList[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        val af = Statements.deleteAllLinksAs(user, programId, observationIds)
        session.prepareR(af.fragment.command).use { p =>
          p.execute(af.argument).as(Result.unit)
        }

      override def setAsterism(
        programId:      Program.Id,
        observationIds: NonEmptyList[Observation.Id],
        targetIds:      Nullable[NonEmptyList[Target.Id]]
      )(using Transaction[F]): F[Result[Unit]] =
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
            ResultT(ADD.fold(Result.unit.pure[F])(insertAsterism(pid, observationIds, _))) *>
              ResultT(DELETE.fold(Result.unit.pure[F]) { tids =>
                val af = Statements.deleteLinksAs(user, pid, observationIds, tids)
                session.prepareR(af.fragment.command).use { p =>
                  p.execute(af.argument).as(Result.unit)
                }
              })
          }.value
        
      override def cloneAsterism(
        originalId: Observation.Id,
        newId: Observation.Id,
      )(using Transaction[F]): F[Unit] =
        val clone = Statements.clone(originalId, newId)
        session.prepareR(clone.fragment.command).use { ps =>
          ps.execute(clone.argument).void
        }

      override def getAsterism(
        programId: Program.Id,
        observationId: Observation.Id
      ): F[List[(Target.Id, Target)]] =
        val af = Statements.getAsterism(user, programId, observationId)
        session.prepareR(af.fragment.query(Decoders.targetDecoder)).use { ps =>
          ps.stream(af.argument, chunkSize = 1024).compile.toList
        }

      override def getAsterisms(
        oids: List[Observation.Id]
      ): F[Map[Observation.Id, List[(Target.Id, Target)]]] =
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

    import ProgramUserService.Statements.{andWhereUserReadAccess, andWhereUserWriteAccess, whereUserWriteAccess}

    def selectProgramId(
      observationIds: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      void"""
        SELECT DISTINCT c_program_id
        FROM t_observation
        WHERE """ |+| observationIdIn(observationIds)

    def insertLinksAs(
      user:           User,
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

      insert |+| values |+| as |+| whereUserWriteAccess(user, programId) |+|
        void""" ON CONFLICT DO NOTHING"""  // the key consists of all the columns anyway
    }

    private def programIdEqual(
      programId: Program.Id
    ): AppliedFragment =
      sql"c_program_id = $program_id"(programId)

    private def observationIdIn(
      observationIds: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      void"c_observation_id IN (" |+|
        observationIds.map(sql"$observation_id").intercalate(void", ") |+|
      void")"

    private def targetIdIn(
      targetIds: NonEmptyList[Target.Id]
    ): AppliedFragment =
      void"c_target_id IN (" |+|
        targetIds.map(sql"$target_id").intercalate(void", ") |+|
      void")"

    def deleteLinksAs(
      user:           User,
      programId:      Program.Id,
      observationIds: NonEmptyList[Observation.Id],
      targetIds:      NonEmptyList[Target.Id]
    ): AppliedFragment =
       void"DELETE FROM ONLY t_asterism_target "        |+|
         void"WHERE " |+| programIdEqual(programId)     |+|
         void" AND " |+| observationIdIn(observationIds) |+|
         void" AND " |+| targetIdIn(targetIds)           |+|
         andWhereUserWriteAccess(user, programId)

    def deleteAllLinksAs(
      user:           User,
      programId:      Program.Id,
      observationIds: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      void"DELETE FROM ONLY t_asterism_target "         |+|
        void"WHERE " |+| programIdEqual(programId)      |+|
        void" AND "  |+| observationIdIn(observationIds) |+|
        andWhereUserWriteAccess(user, programId)

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
        AND t_target.c_existence = 'present' -- don't clone references to deleted targets
      """.apply(newOid, originalOid)

    def getAsterism(user: User, pid: Program.Id, oid: Observation.Id): AppliedFragment =
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
          c_source_profile
        from t_target t
        inner join t_asterism_target a
        on t.c_target_id = a.c_target_id
          and a.c_program_id = $program_id
          and a.c_observation_id = $observation_id
        where t.c_existence = 'present'
      """.apply(pid, oid) |+| andWhereUserReadAccess(user, pid)

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
          c_source_profile
        from t_target t
        inner join t_asterism_target a
        on t.c_target_id = a.c_target_id
          and a.c_observation_id in ($enc)
        where t.c_existence = 'present'
      """.query(observation_id ~ Decoders.targetDecoder)

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
            .toRight(s"Invalid target $id.")
      }

  }
}
