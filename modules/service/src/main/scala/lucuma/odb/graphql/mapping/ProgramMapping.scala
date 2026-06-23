// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.Monad
import cats.effect.Resource
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegShort
import grackle.Predicate
import grackle.Predicate.*
import grackle.Query.*
import grackle.QueryCompiler.Elab
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.model.Attachment
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.ProgramNote
import lucuma.core.model.ProgramUser
import lucuma.core.model.User
import lucuma.core.model.sequence.BandedTime
import lucuma.core.model.sequence.CategorizedTimeRange
import lucuma.core.util.CalculatedValue
import lucuma.itc.client.ItcClient
import lucuma.odb.Config
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.json.calculatedValue.given
import lucuma.odb.json.time.query.given
import lucuma.odb.json.timeaccounting.given
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.Services
import org.http4s.client.Client

import Services.Syntax.*
import binding.*
import table.*

trait ProgramMapping[F[_]]
  extends ProgramView[F]
     with UserTable[F]
     with ProgramNoteTable[F]
     with ProgramUserView[F]
     with ProposalView[F]
     with ObservationView[F]
     with AttachmentTable[F]
     with Predicates[F]
     with ProgramReferenceView[F]
     with ResultMapping[F]
     with GroupElementView[F]
     with UserInvitationTable[F]
     with KeyValueEffectHandler[F]
     with AllocationTable[F] {

  def user: User
  def itcClient: ItcClient[F]
  def services: Resource[F, Services[F]]
  def commitHash: CommitHash
  def timeEstimateCalculator: TimeEstimateCalculatorImplementation.ForInstrumentMode
  def emailConfig: Config.Email
  def httpClient: Client[F]

  lazy val ProgramMapping: ObjectMapping =
    ObjectMapping(ProgramType)(
      SqlField("id", ProgramView.Id, key = true),
      SqlField("existence", ProgramView.Existence),
      SqlField("name", ProgramView.Name),
      SqlField("description", ProgramView.Description),

      SqlObject("notes", Join(ProgramView.Id, ProgramNoteTable.ProgramId)),

      SqlField("type", ProgramView.ProgramType),
      SqlObject("reference",  Join(ProgramView.Id, ProgramReferenceView.Id)),

      SqlField("proposalStatus", ProgramView.ProposalStatus),
      SqlObject("pi", Join(ProgramView.Id, ProgramUserView.ProgramId)),
      SqlObject("users", Join(ProgramView.Id, ProgramUserView.ProgramId)),
      SqlObject("observations"),
      SqlObject("configurationRequests"),
      SqlObject("proposal", Join(ProgramView.Id, ProposalView.ProgramId)),
      SqlObject("active"),
      SqlObject("groupElements", Join(ProgramView.Id, GroupElementView.ProgramId)),
      SqlObject("allGroupElements", Join(ProgramView.Id, GroupElementView.ProgramId)),
      SqlObject("attachments", Join(ProgramView.Id, AttachmentTable.ProgramId)),

      EffectField("timeEstimateRange", estimateRangeHandler, List("id")),
      EffectField("timeEstimateBanded", estimateBandedHandler, List("id")),

      EffectField("timeCharge", timeChargeHandler, List("id")),
      SqlObject("userInvitations", Join(ProgramView.Id, UserInvitationTable.ProgramId)),
      SqlObject("allocations", Join(ProgramView.Id, AllocationTable.ProgramId)),
      SqlField("calibrationRole", ProgramView.CalibrationRole),
      SqlObject("goa"),
      SqlField("resourceLimit", ProgramView.ResourceLimit),
      SqlField("resourceCount", ProgramView.ResourceCount)
    )

  lazy val ProgramElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {

    case (ProgramType, "pi", Nil) =>
      Elab.transformChild { child =>
        Unique(Filter(Predicates.programUser.isPi, child))
      }

    case (ProgramType, "users", Nil) =>
      Elab.transformChild: child =>
        FilterOrderByOffsetLimit(
          pred   = Predicates.programUser.isNotPi.some,
          oss    = List(OrderSelection[ProgramUser.Id](ProgramUserType / "id")).some,
          offset = none,
          limit  = none,
          child
        )

    case (ProgramType, "notes", List(
      BooleanBinding("includeDeleted", rIncludeDeleted)
    )) =>
      Elab.transformChild: child =>
        rIncludeDeleted.map: includeDeleted =>
          OrderBy(
            OrderSelections(List(
              OrderSelection[ProgramNote.Id](ProgramNoteType / "id")
            )),
            Filter(
              Predicate.And(
                Predicates.programNote.existence.includeDeleted(includeDeleted),
                Predicates.programNote.isVisibleTo(user)
              ),
              child
            )
          )

    case (ProgramType, "observations", List(
      BooleanBinding("includeDeleted", rIncludeDeleted),
      ObservationIdBinding.Option("OFFSET", rOFFSET),
      NonNegIntBinding.Option("LIMIT", rLIMIT),
    )) =>
      Elab.transformChild { child =>
        (rIncludeDeleted, rOFFSET, rLIMIT).parTupled.flatMap { (includeDeleted, OFFSET, lim) =>
          val limit = lim.fold(ResultMapping.MaxLimit)(_.value)
          ResultMapping.selectResult(child, limit) { q =>
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
      }

    case (ProgramType, "configurationRequests", List(
      ConfigurationRequestIdBinding.Option("OFFSET", rOFFSET),
      NonNegIntBinding.Option("LIMIT", rLIMIT),
    )) =>
      Elab.transformChild { child =>
        (rOFFSET, rLIMIT).parTupled.flatMap { (OFFSET, lim) =>
          val limit = lim.fold(ResultMapping.MaxLimit)(_.value)
          ResultMapping.selectResult(child, limit) { q =>
            FilterOrderByOffsetLimit(
              pred = Some(
                OFFSET.fold[Predicate](True)(Predicates.configurationRequest.id.gtEql)
              ),
              oss = Some(List(OrderSelection[ConfigurationRequest.Id](ConfigurationRequestType / "id", true, true))),
              offset = None,
              limit = Some(limit + 1),
              q
            )
          }
        }
      }

    case (ProgramType, "groupElements", List(
      BooleanBinding("includeDeleted", rIncludeDeleted),
    )) =>
      Elab.transformChild { child =>
        rIncludeDeleted.map: includeDeleted =>
          FilterOrderByOffsetLimit(
            pred = And(
              Predicates.groupElement.parentGroupId.isNull(true),
              Predicates.groupElement.existence.includeDeleted(includeDeleted)
            ).some,
            oss = Some(List(OrderSelection[NonNegShort](GroupElementType / "parentIndex", true, true))),
            offset = None,
            limit = None,
            child
          )
      }

    case (ProgramType, "allGroupElements", List(
      BooleanBinding("includeDeleted", rIncludeDeleted),
    )) =>
      Elab.transformChild { child =>
        rIncludeDeleted.map: includeDeleted =>
          FilterOrderByOffsetLimit(
            pred = Some(Predicates.groupElement.existence.includeDeleted(includeDeleted)),
            oss = Some(List(
              OrderSelection[Option[Group.Id]](GroupElementType / "parentGroupId", true, true),
              OrderSelection[NonNegShort](GroupElementType / "parentIndex", true, true)
            )),
            offset = None,
            limit = None,
            child
          )
        }

    case (ProgramType, "attachments", Nil) =>
      Elab.transformChild { child =>
        OrderBy(OrderSelections(List(OrderSelection[Attachment.Id](AttachmentType / "id"))), child)
      }

    case (ProgramType, "allocations", Nil) =>
      Elab.transformChild { child =>
        OrderBy(OrderSelections(List(
          OrderSelection[ScienceBand](AllocationType / "scienceBand"),
          OrderSelection[TimeAccountingCategory](AllocationType / "category")
        )), child)
      }

  }

  private lazy val estimateRangeHandler: EffectHandler[F] =
    keyValueEffectHandler[Program.Id, Option[CalculatedValue[CategorizedTimeRange]]]("id"): pid =>
      services.useTransactionally:
        timeEstimateService
          .estimateProgramRange(pid)

  private lazy val estimateBandedHandler: EffectHandler[F] =
    keyValueEffectHandler[Program.Id, List[CalculatedValue[BandedTime]]]("id"): gid =>
      services.useTransactionally:
        timeEstimateService
          .estimateProgramBanded(gid)
          .map(_.toList.sortBy(_._1).map((b, cv) => Monad[CalculatedValue].map(cv)(t => BandedTime(b, t))))

  private val timeChargeHandler: EffectHandler[F] =
    keyValueEffectHandler[Program.Id, List[BandedTime]]("id") { pid =>
      services.useTransactionally {
        Services.asSuperUser:
          timeAccountingService.selectProgram(pid)
      }
    }

}
