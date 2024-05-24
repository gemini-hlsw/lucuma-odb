// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.effect.Resource
import cats.syntax.eq.*
import cats.syntax.traverse.*
import grackle.Cursor
import grackle.Query
import grackle.Query.Binding
import grackle.Query.EffectHandler
import grackle.Query.OrderBy
import grackle.Query.OrderSelection
import grackle.Query.OrderSelections
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.ResultT
import grackle.TypeRef
import io.circe.syntax.*
import lucuma.core.model.CallForProposals
import lucuma.core.model.User
import lucuma.core.util.Timestamp
import lucuma.odb.data.Tag
import lucuma.odb.graphql.binding.TagBinding
import lucuma.odb.graphql.table.CallForProposalsView
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.syntax.resultT.*

trait CallForProposalsMapping[F[_]] extends CallForProposalsView[F] {

  def user: User
  def services: Resource[F, Services[F]]

  lazy val CallForProposalsPartnerMapping: TypeMapping =
    ObjectMapping(CallForProposalsPartnerType)(
      SqlField("id",                 CallForProposalsPartnerTable.CfpId, hidden = true, key = true),
      SqlField("partner",            CallForProposalsPartnerTable.Partner, key = true),
      SqlField("submissionDeadline", CallForProposalsPartnerTable.Deadline)
    )

  lazy val CallForProposalsMapping: TypeMapping =
    ObjectMapping(CallForProposalsType)(
      SqlField("id",       CallForProposalsView.Id, key = true),
      SqlField("title",    CallForProposalsView.Title),
      SqlField("type",     CallForProposalsView.Type),
      SqlField("semester", CallForProposalsView.Semester),
      SqlObject("raLimitStart"),
      SqlObject("raLimitEnd"),
      SqlObject("decLimitStart"),
      SqlObject("decLimitEnd"),
      EffectField("submissionDeadline", deadlineHandler, List("id")),
      SqlObject("active"),
      SqlObject("partners",   Join(CallForProposalsView.Id, CallForProposalsPartnerTable.CfpId)),
      SqlField("instruments", CallForProposalsView.Instruments),
      SqlField("existence",   CallForProposalsView.Existence),
      SqlField("_isOpen",     CallForProposalsView.IsOpen, hidden = true)
    )

  lazy val CallForProposalsElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {
    case (CallForProposalsType, "partners", Nil) =>
      Elab.transformChild { child =>
        OrderBy(
          OrderSelections(
            List(
              OrderSelection[Tag](CallForProposalsPartnerType / "partner")
            )
          ),
          child
        )
      }

    case (CallForProposalsType, "submissionDeadline", List(TagBinding.Option("partner", rPartner))) =>
      Elab.liftR(rPartner).flatMap { partner =>
        Elab.env("partner" -> partner)
      }
  }

  lazy val deadlineHandler: EffectHandler[F] =
    new EffectHandler[F] {
      def queryContext(queries: List[(Query, Cursor)]): Result[List[(CallForProposals.Id, Option[Tag])]] =
        queries.traverse { case (_, cursor) =>
          for {
            c <- cursor.fieldAs[CallForProposals.Id]("id")
            p <- cursor.fullEnv.getR[Option[Tag]]("partner")
          } yield (c, p)
        }

      def runEffects(queries: List[(Query, Cursor)]): F[Result[List[Cursor]]] =
        (for {
          ctx <- ResultT.fromResult(queryContext(queries))
          sds <- ctx.distinct.traverse { case (cid, partner) =>
                   ResultT.liftF(services.useTransactionally {
                     proposalService.submissionDeadline(cid, partner)
                   }).map((cid, partner, _))
                 }
          res <- ResultT.fromResult(
                   ctx.flatMap { case (cid, partner) => sds.find(r => r._1 === cid && r._2 === partner).map(_._3).toList }
                      .zip(queries)
                      .traverse { case (result, (query, parentCursor)) =>
                        Query.childContext(parentCursor.context, query).map { childContext =>
                          CirceCursor(childContext, result.asJson, Some(parentCursor), parentCursor.fullEnv)
                        }
                      }
                 )
        } yield res).value
    }

}
