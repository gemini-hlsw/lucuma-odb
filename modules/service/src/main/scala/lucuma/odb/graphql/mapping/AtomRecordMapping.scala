// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Query.Binding
import grackle.QueryCompiler.Elab
import grackle.TypeRef
import lucuma.core.model.User
import lucuma.odb.graphql.binding.NonNegIntBinding
import lucuma.odb.graphql.binding.TimestampBinding
import lucuma.odb.graphql.predicate.Predicates

import table.AtomRecordTable
import table.StepRecordTable
import table.VisitTable

trait AtomRecordMapping[F[_]] extends AtomRecordTable[F]
                                 with Predicates[F]
                                 with SelectSubquery
                                 with StepRecordTable[F]
                                 with VisitTable[F] {
  def user: User

  lazy val AtomRecordMapping: ObjectMapping =
    ObjectMapping(
      tpe           = AtomRecordType,
      fieldMappings = List(
        SqlField("id",           AtomRecordTable.Id, key = true),
        SqlField("instrument",   AtomRecordTable.Instrument),
        SqlObject("visit",       Join(AtomRecordTable.VisitId, VisitTable.Id)),
        SqlField("created",      AtomRecordTable.Created),
        SqlField("sequenceType", AtomRecordTable.SequenceType),
        SqlField("stepCount",    AtomRecordTable.StepCount),
        SqlObject("steps")
      )
    )

  lazy val AtomRecordElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {

    case (AtomRecordType, "steps", List(
      TimestampBinding.Option("OFFSET", rOFFSET),
      NonNegIntBinding.Option("LIMIT", rLIMIT)
    )) =>
      selectWithOffsetAndLimit(rOFFSET, rLIMIT, StepRecordType, "created", Predicates.stepRecord.created, Predicates.stepRecord.atomRecord.visit.observation.program)

  }

}