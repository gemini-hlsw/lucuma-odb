// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.syntax.all._
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.Result
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.skunk.SkunkMapping
import eu.timepit.refined.types.numeric.NonNegShort
import lucuma.core.model.Group
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.odb.data.Tag
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.GroupElementView

import binding._
import table._

trait ProgramMapping[F[_]]
  extends ProgramTable[F]
     with UserTable[F]
     with ProgramUserTable[F]
     with ProposalTable[F]
     with ObservationView[F]
     with ObsAttachmentTable[F]
     with Predicates[F]
     with ProposalAttachmentTable[F]
     with ResultMapping[F]
     with GroupElementView[F] {

  def user: User

  lazy val ProgramMapping: ObjectMapping =
    ObjectMapping(
      tpe = ProgramType,
      fieldMappings = List(
        SqlField("id", ProgramTable.Id, key = true),
        SqlField("existence", ProgramTable.Existence, hidden = true),
        SqlField("name", ProgramTable.Name),
        SqlField("piUserId", ProgramTable.PiUserId, hidden = true),
        SqlObject("pi", Join(ProgramTable.PiUserId, UserTable.UserId)),
        SqlObject("users", Join(ProgramTable.Id, ProgramUserTable.ProgramId)),
        SqlObject("observations"),
        SqlObject("proposal", Join(ProgramTable.Id, ProposalTable.ProgramId)),
        SqlObject("groupElements", Join(ProgramTable.Id, GroupElementView.ProgramId)),
        SqlObject("allGroupElements", Join(ProgramTable.Id, GroupElementView.ProgramId)),
        SqlObject("obsAttachments", Join(ProgramTable.Id, ObsAttachmentTable.ProgramId)),
        SqlObject("proposalAttachments", Join(ProgramTable.Id, ProposalAttachmentTable.ProgramId))
      ),
    )

  lazy val ProgramElaborator: Map[TypeRef, PartialFunction[Select, Result[Query]]] =
    Map(
      ProgramType -> {
        case Select("observations", List(
          BooleanBinding("includeDeleted", rIncludeDeleted),
          ObservationIdBinding.Option("OFFSET", rOFFSET),
          NonNegIntBinding.Option("LIMIT", rLIMIT),
        ), child) =>
          (rIncludeDeleted, rOFFSET, rLIMIT).parTupled.flatMap { (includeDeleted, OFFSET, lim) =>
            val limit = lim.fold(ResultMapping.MaxLimit)(_.value)
            ResultMapping.selectResult("observations", child, limit) { q =>
              FilterOrderByOffsetLimit(
                pred = Some(and(List(
                  Predicates.observation.existence.includeDeleted(includeDeleted),
                  OFFSET.fold[Predicate](True)(Predicates.observation.id.gtEql)
                ))),
                oss = Some(List(OrderSelection[Observation.Id](ObservationType / "id", true, true))),
                offset = None,
                limit = Some(limit + 1),
                q
              )
            }
          }
        case Select("groupElements", Nil, child) =>
          Result(
            Select("groupElements", Nil,
              FilterOrderByOffsetLimit(
                pred = Some(Predicates.groupElement.parentGroupId.isNull(true)),
                oss = Some(List(OrderSelection[NonNegShort](GroupElementType / "parentIndex", true, true))),
                offset = None,
                limit = None,
                child
              )
            )
          )
        case Select("allGroupElements", Nil, child) =>
          Result(
            Select("allGroupElements", Nil,
              FilterOrderByOffsetLimit(
                pred = None,
                oss = Some(List(
                  OrderSelection[Option[Group.Id]](GroupElementType / "parentGroupId", true, true),
                  OrderSelection[NonNegShort](GroupElementType / "parentIndex", true, true)
                )),
                offset = None,
                limit = None,
                child
              )
            )
          )
        case Select("obsAttachments", Nil, child) =>
          Result(
            Select("obsAttachments", Nil,
              OrderBy(OrderSelections(List(OrderSelection[ObsAttachment.Id](ObsAttachmentType / "id"))), child)
            )
          )
        case Select("proposalAttachments", Nil, child) =>
          Result(
            Select("proposalAttachments", Nil,
              OrderBy(OrderSelections(List(OrderSelection[Tag](ProposalAttachmentType / "attachmentType"))), child)
            )
          )
      }
    )

}

