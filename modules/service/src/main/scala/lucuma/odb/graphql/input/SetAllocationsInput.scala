// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.ScienceBand
import lucuma.core.model.Program
import lucuma.odb.graphql.binding.*

case class SetAllocationsInput(
  programId:   Program.Id,
  allocations: List[AllocationInput]
) {

  def bands: Set[ScienceBand] =
    allocations.map(_.scienceBand).toSet

}

object SetAllocationsInput {

  val Binding: Matcher[SetAllocationsInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ProgramIdBinding("programId", rProgramId),
        AllocationInput.Binding.List("allocations", rAllocations)
      ) =>
        val rValidAllocations = rAllocations.flatMap { allocations =>
          Matcher
            .validationFailure("Each category + band combination may only appear once.")
            .unlessA(allocations.map(a => (a.category, a.scienceBand)).toSet.size === allocations.size)
            .as(allocations)
        }
        (rProgramId, rValidAllocations).mapN(apply)
    }

}
