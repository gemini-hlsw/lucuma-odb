// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.effect.Resource
import cats.syntax.apply.*
import eu.timepit.refined.cats.given
import grackle.Query.Binding
import grackle.QueryCompiler.Elab
import grackle.TypeRef
import io.circe.syntax.*
import lucuma.core.model.User
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.graphql.binding.NonNegIntBinding
import lucuma.odb.graphql.binding.PosIntBinding
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.json.time.query.given
import lucuma.odb.service.Services

import table.AtomRecordView
import table.StepRecordView
import table.VisitTable

trait AtomRecordMapping[F[_]] extends AtomRecordView[F]
                                 with EventRangeEffectHandler[F]
                                 with Predicates[F]
                                 with SelectSubquery
                                 with StepRecordView[F]
                                 with VisitTable[F]:
  def user: User
  def services: Resource[F, Services[F]]

  lazy val AtomRecordMapping: ObjectMapping =
    ObjectMapping(AtomRecordType)(
      SqlField("id",              AtomRecordView.Id, key = true),
      SqlField("index",           AtomRecordView.AtomIndex),
      SqlField("description",     AtomRecordView.Description),
      SqlField("instrument",      AtomRecordView.Instrument),
      SqlObject("visit",          Join(AtomRecordView.VisitId, VisitTable.Id)),
      SqlField("executionState",  AtomRecordView.ExecutionState),
      SqlField("_firstEventTime", AtomRecordView.FirstEventTime, hidden = true),
      SqlField("_lastEventTime",  AtomRecordView.LastEventTime, hidden = true),
      CursorFieldJson(
        "interval",
        cursor =>
          for
            s <- cursor.fieldAs[Option[Timestamp]]("_firstEventTime")
            e <- cursor.fieldAs[Option[Timestamp]]("_lastEventTime")
          yield (s, e).mapN { (ts, te) => TimestampInterval.between(ts, te) }.asJson,
        List("_firstEventTime", "_lastEventTime")
      ),
      SqlField("sequenceType",   AtomRecordView.SequenceType),
      SqlObject("steps")
    )

  lazy val AtomRecordElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {

    case (AtomRecordType, "steps", List(
      PosIntBinding.Option("OFFSET", rOFFSET),
      NonNegIntBinding.Option("LIMIT", rLIMIT)
    )) =>
      selectWithOffsetAndLimit(rOFFSET, rLIMIT, StepRecordType, "index", Predicates.stepRecord.index, Predicates.stepRecord.atomRecord.visit.observation.program)

  }