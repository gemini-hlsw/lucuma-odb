// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.effect.Resource
import grackle.Query.Binding
import grackle.Query.EffectHandler
import grackle.QueryCompiler.Elab
import grackle.TypeRef
import lucuma.core.model.User
import lucuma.core.model.sequence.Atom
import lucuma.odb.graphql.binding.NonNegIntBinding
import lucuma.odb.graphql.binding.TimestampBinding
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*

import table.AtomRecordTable
import table.StepRecordView
import table.VisitTable

trait AtomRecordMapping[F[_]] extends AtomRecordTable[F]
                                 with EventRangeEffectHandler[F]
                                 with Predicates[F]
                                 with SelectSubquery
                                 with StepRecordView[F]
                                 with VisitTable[F] {
  def user: User
  def services: Resource[F, Services[F]]

  lazy val AtomRecordMapping: ObjectMapping =
    ObjectMapping(AtomRecordType)(
      SqlField("id",             AtomRecordTable.Id, key = true),
      SqlField("instrument",     AtomRecordTable.Instrument),
      SqlObject("visit",         Join(AtomRecordTable.VisitId, VisitTable.Id)),
      SqlField("created",        AtomRecordTable.Created),
      SqlField("executionState", AtomRecordTable.ExecutionState),
      EffectField("interval",    intervalHandler, List("id")),
      SqlField("sequenceType",   AtomRecordTable.SequenceType),
      SqlField("generatedId",    AtomRecordTable.GeneratedId),
      SqlField("idempotencyKey", AtomRecordTable.IdempotencyKey),
      SqlObject("steps")
    )

  lazy val AtomRecordElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {

    case (AtomRecordType, "steps", List(
      TimestampBinding.Option("OFFSET", rOFFSET),
      NonNegIntBinding.Option("LIMIT", rLIMIT)
    )) =>
      selectWithOffsetAndLimit(rOFFSET, rLIMIT, StepRecordType, "created", Predicates.stepRecord.created, Predicates.stepRecord.atomRecord.visit.observation.program)

  }

  private lazy val intervalHandler: EffectHandler[F] =
    eventRangeEffectHandler[Atom.Id]("id", services, executionEventService.atomRange)

}