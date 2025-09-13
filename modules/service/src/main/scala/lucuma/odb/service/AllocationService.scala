// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.ScienceBand
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.syntax.string.*
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.input.AllocationInput
import lucuma.odb.graphql.mapping.AccessControl
import lucuma.odb.service.Services.SuperUserAccess
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.implicits.*

import Services.Syntax.*

trait AllocationService[F[_]] {

  /**
   * Selects the science bands for which time has been allocated, if any, for
   * the given program.
   */
  def selectScienceBands(pid: Program.Id)(using SuperUserAccess): F[Set[ScienceBand]]

  def setAllocations(input: AccessControl.CheckedWithId[List[AllocationInput], Program.Id])(using Transaction[F]): F[Result[Program.Id]]

  /**
   * Validates that the given `band` may be assigned to observations in the
   * programs referenced by `pids`.
   */
  def validateBand(band: ScienceBand, pids: List[Program.Id])(using SuperUserAccess): F[Result[Unit]]
}

object AllocationService {

  def instantiate[F[_]: MonadCancelThrow](using Services[F]): AllocationService[F] =
    new AllocationService[F] {

      def selectScienceBands(pid: Program.Id)(using SuperUserAccess): F[Set[ScienceBand]] =
        session.execute(Statements.SelectScienceBands)(pid).map(_.toSet)

      def validateBand(band: ScienceBand, pids: List[Program.Id])(using SuperUserAccess): F[Result[Unit]] =
        NonEmptyList.fromList(pids).fold(Result.unit.pure[F]) { nelPids =>
          session.execute(Statements.validPidsForBand(nelPids))((band, nelPids)).map { validPids =>
            (pids.toSet &~ validPids.toSet).toList match {
              case Nil => Result.unit  // there are no `pids` not in `validPids`
              case ps  => OdbError.InvalidArgument(s"One or more programs have not been allocated time in ${band.tag.toScreamingSnakeCase}: ${pids.mkString(", ")}".some).asFailure
            }
          }
        }

      override def setAllocations(input: AccessControl.CheckedWithId[List[AllocationInput], Program.Id])(using Transaction[F]): F[Result[Program.Id]] =
        input.foldWithId(OdbError.InvalidArgument().asFailureF) { (allocations, pid) =>
          val bands = allocations.map(_.scienceBand).toSet
          deleteAllocations(pid) *>
          NonEmptyList.fromList(allocations).traverse_ { lst =>
            session.execute(Statements.setAllocations(lst))((pid, lst))
          }                                  *>
          session
            .execute(Statements.SetScienceBandWhereNull)(pid, bands.head)
            .whenA(bands.sizeIs == 1)  *>
          validateObservations(bands, pid).map(_.as(pid))
        }

      @annotation.nowarn("msg=unused implicit parameter")
      private def deleteAllocations(pid: Program.Id)(using Transaction[F]): F[Unit] =
        session.execute(Statements.DeleteAllocations)(pid).void

      // If any observations have already been assigned bands which are not in
      // the given SetAllocationsInput, then generate a warning result listing
      // them.
      @annotation.nowarn("msg=unused implicit parameter")
      private def validateObservations(bands: Set[ScienceBand], pid: Program.Id)(using Transaction[F]): F[Result[Unit]] =
        val noneBands  = session.execute(Statements.ObservationsWithNonNullBand)(pid)
        NonEmptyList.fromList(bands.toList).fold(noneBands) { nel =>
          session.execute(Statements.invalidObservationsForBands(nel))(pid, nel)
        }.map {
          case Nil => Result.unit
          case os  => OdbError.InvalidArgument(s"The following observations now have a science band for which no time is allocated: ${os.mkString(", ")}".some).asWarning(())
        }
    }

  object Statements {

    val SelectScienceBands: Query[Program.Id, ScienceBand] =
      sql"""
        SELECT DISTINCT c_science_band
        FROM t_allocation
        WHERE c_program_id = $program_id
        AND c_science_band IS NOT NULL
        AND c_duration > INTERVAL '0'
      """.query(science_band)

    /**
     * Obtains the subset of `pids` to which a given science band may be legally
     * assigned.
     */
    def validPidsForBand(pids: NonEmptyList[Program.Id]): Query[(ScienceBand, pids.type), Program.Id] =
      sql"""
        SELECT c_program_id
        FROM t_allocation
        WHERE c_science_band = $science_band
          AND c_duration > INTERVAL '0'
          AND c_program_id IN (${program_id.values.list(pids.size)})
      """.query(program_id).contramap { (band, nel) => (band, nel.toList) }

    def setAllocations(allocations: NonEmptyList[AllocationInput]): Command[(Program.Id, allocations.type)] =
      sql"""
        INSERT INTO t_allocation (c_program_id, c_ta_category, c_science_band, c_duration)
        VALUES ${(program_id *: time_accounting_category *: science_band *: time_span).values.list(allocations.size)}
      """.command
         .contramap {
           (pid, allocs) => allocs.toList.map { a => (pid, a.category, a.scienceBand, a.duration) }
         }

    val DeleteAllocations: Command[Program.Id] =
      sql"""
        DELETE FROM t_allocation
        WHERE t_allocation.c_program_id = $program_id
      """.command

    val ObservationsWithNonNullBand: Query[Program.Id, Observation.Id] =
      sql"""
        SELECT c_observation_id
        FROM t_observation
        WHERE c_program_id = $program_id
          AND c_science_band IS NOT NULL
      """.query(observation_id)

    def invalidObservationsForBands(validBands: NonEmptyList[ScienceBand]): Query[(Program.Id, validBands.type), Observation.Id] =
      sql"""
        SELECT c_observation_id
        FROM t_observation
        WHERE c_program_id = $program_id
          AND c_science_band NOT IN (${science_band.values.list(validBands.size)})
      """.query(observation_id).contramap { (pid, nel) => (pid, nel.toList) }

    // Sets all the observations with NULL science bands to the given value.
    val SetScienceBandWhereNull: Query[(Program.Id, ScienceBand), Observation.Id] =
      sql"""
        UPDATE t_observation
        SET c_science_band = $science_band
        WHERE c_program_id = $program_id
        AND c_science_band IS NULL
        RETURNING c_observation_id
      """.query(observation_id)
         .contramap { (s, p) => (p, s) }
  }

}
