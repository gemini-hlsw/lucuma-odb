// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.Partner
import lucuma.core.model.Program
import lucuma.core.syntax.string.*
import lucuma.core.util.TimeSpan
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.data.ScienceBand
import lucuma.odb.graphql.input.AllocationInput
import lucuma.odb.graphql.input.SetAllocationsInput
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.implicits.*

import Services.Syntax.*

trait AllocationService[F[_]] {
  def setAllocations(input: SetAllocationsInput)(using Transaction[F], Services.StaffAccess): F[Unit]

  /** Validats that the given `band` may be assigned to these `pids`. */
  def validateBand(band: ScienceBand, pids: List[Program.Id]): F[Result[Unit]]
}

object AllocationService {

  def instantiate[F[_]: MonadCancelThrow](using Services[F]): AllocationService[F] =
    new AllocationService[F] {

      def validateBand(band: ScienceBand, pids: List[Program.Id]): F[Result[Unit]] =
        NonEmptyList.fromList(pids).fold(Result.unit.pure[F]) { nelPids =>
          session.execute(Statements.validPidsForBand(nelPids))((band, nelPids)).map { validPids =>
            (pids.toSet &~ validPids.toSet).toList match {
              case Nil => Result.unit  // there are no `pids` not in `validPids`
              case ps  => OdbError.InvalidArgument(s"One or more programs have not been allocated time in ${band.tag.toScreamingSnakeCase}: ${pids.mkString("[", ", ", "]")}".some).asFailure
            }
          }
        }

      def deleteAllocations(pid: Program.Id)(using Transaction[F]): F[Unit] =
        session.execute(Statements.DeleteAllocations)(pid).void

      def setAllocations(input: SetAllocationsInput)(using Transaction[F], Services.StaffAccess): F[Unit] =
        deleteAllocations(input.programId) *>
        NonEmptyList.fromList(input.allocations).traverse_ { lst =>
          session.execute(Statements.setAllocations(lst))((input.programId, lst))
        }                                  *>
        observationService
          .setScienceBandInAllObservationsNoValidation(input.programId, input.bands.head)
          .whenA(input.bands.sizeIs == 1)

    }

  object Statements {

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
        INSERT INTO t_allocation (c_program_id, c_partner, c_science_band, c_duration)
        VALUES ${(program_id *: partner *: science_band *: time_span).values.list(allocations.size)}
      """.command
         .contramap {
           (pid, allocs) => allocs.toList.map { a => (pid, a.partner, a.scienceBand, a.duration) }
         }

    val DeleteAllocations: Command[Program.Id] =
      sql"""
        DELETE FROM t_allocation
        WHERE t_allocation.c_program_id = $program_id
      """.command

  }

}
