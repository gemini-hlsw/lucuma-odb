// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.graphql

import cats.syntax.all.*
import grackle.Predicate
import grackle.Predicate.*
import grackle.Query.*
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import lucuma.core.enums.Site
import lucuma.core.model.Semester
import lucuma.core.util.Timestamp
import lucuma.odb.graphql.binding.*
import org.typelevel.cats.time.*
import resource.model.TelescopeSubsystem

import java.time.temporal.ChronoUnit

trait QueryMapping[F[_]]
    extends TimestampIntervalMapping[F]
    with TelescopeNightMapping[F]
    with ComponentCatalogMapping[F]
    with PublishedSemesterMapping[F] {

  lazy val QueryMapping: ObjectMapping =
    ObjectMapping(QueryType)(
      SqlObject("publishedSemesters"),
      SqlObject("telescopeAvailability"),
      SqlObject("telescopeMode"),
      SqlObject("tooSupport"),
      SqlObject("instrumentAvailability"),
      SqlObject("telescopeSubsystemAvailability"),
      SqlObject("instrumentComponentAvailability"),
      SqlObject("components"),
      RootEffect("telescopeNight")(telescopeNightHandler),
      RootEffect("telescopeNights")(telescopeNightsHandler)
    )

  lazy val QueryElaborator: ElaboratorPF = List(
    PublishedSemestersE,
    blockQuery("telescopeAvailability", TelescopeAvailabilityBlockType),
    blockQuery("telescopeMode", TelescopeModeBlockType),
    blockQuery("tooSupport", TooSupportBlockType),
    blockQuery("instrumentAvailability", InstrumentAvailabilityBlockType),
    blockQuery("telescopeSubsystemAvailability",
               TelescopeSubsystemAvailabilityBlockType,
               subsystemFilter
    ),
    blockQuery("instrumentComponentAvailability",
               InstrumentComponentAvailabilityBlockType,
               componentFilter
    ),
    TelescopeNightElaborator,
    ComponentsElaborator
  ).combineAll

  private lazy val PublishedSemestersE: ElaboratorPF =
    case (QueryType, "publishedSemesters", Nil)     =>
      Elab.transformChild { child =>
        OrderBy(
          OrderSelections(
            List(
              OrderSelection[Site](PublishedSemesterType / "site"),
              OrderSelection[Semester](PublishedSemesterType / "semester")
            )
          ),
          child
        )
      }
    case (PublishedSemesterType, "moonEvents", Nil) =>
      Elab.transformChild { child =>
        OrderBy(OrderSelections(List(OrderSelection[java.time.LocalDate](MoonEventType / "date"))),
                child
        )
      }

  /**
   * Elaborator for a block interval query: filters to blocks overlapping [start, end) at the site,
   * orders by start, and (when clip is set) puts the window in the Env for the interval
   * CursorFields to trim against. The `extra` function receives the bindings that follow the four
   * common arguments and contributes an additional filter predicate.
   */
  protected def blockQuery(
    fieldName: String,
    tpe:       TypeRef,
    extra:     List[Binding] => Result[List[Predicate]] = _ => Result(Nil)
  ): ElaboratorPF = {
    case (QueryType,
          `fieldName`,
          SiteBinding("site", rSite) ::
          TimestampBinding("start", rStart) ::
          TimestampBinding("end", rEnd) ::
          BooleanBinding("clip", rClip) ::
          rest
        ) =>
      for {
        args                    <- Elab.liftR((rSite, rStart, rEnd, rClip).parTupled)
        (site, start, end, clip) = args
        _                       <- Elab.liftR(
                                     validateWindow(
                                       start < end,
                                       end.toInstant.isAfter(start.toInstant.plus(MaxWindow, ChronoUnit.DAYS)),
                                       "days"
                                     )
                                   )
        extraPreds              <- Elab.liftR(extra(rest))
        _                       <- if (clip) Elab.env(ClipStartKey -> start, ClipEndKey -> end)
                                   else Elab.unit
        _                       <- Elab.transformChild { child =>
                                     FilterOrderByOffsetLimit(
                                       pred = Some(
                                         Predicate.and(
                                           Eql(tpe / "site", Const(site)) ::
                                             Lt(tpe / StartField, Const(end)) ::
                                             Gt(tpe / EndField, Const(start)) ::
                                             extraPreds
                                         )
                                       ),
                                       oss = Some(
                                         List(
                                           OrderSelection[Timestamp](tpe / StartField),
                                           OrderSelection[Long](tpe / IdField)
                                         )
                                       ),
                                       offset = None,
                                       limit = None,
                                       child = child
                                     )
                                   }
      } yield ()
  }

  /** Input binding by raw tag: the tags are the GraphQL values by convention. */
  private val TelescopeSubsystemBinding: Matcher[TelescopeSubsystem] =
    enumeratedBinding

  /** Optional `subsystems` filter that follows the common block arguments. */
  private def subsystemFilter(rest: List[Binding]): Result[List[Predicate]] =
    rest match
      case List(TelescopeSubsystemBinding.List.Option("subsystems", rSubs)) =>
        rSubs.map(_.map(In(TelescopeSubsystemAvailabilityBlockType / "subsystem", _)).toList)
      case _                                                                =>
        Result.internalError("Unexpected arguments for 'telescopeSubsystemAvailability'.")

  /** Optional `instruments` and `componentTypes` filters that follow the common block arguments. */
  private def componentFilter(rest: List[Binding]): Result[List[Predicate]] =
    val tpe = InstrumentComponentAvailabilityBlockType
    rest match
      case List(ResourceInstrumentBinding.List.Option("instruments", rInsts),
                InstrumentComponentTypeBinding.List.Option("componentTypes", rTypes)
          ) =>
        (rInsts, rTypes).parMapN { (insts, types) =>
          List(
            insts.map(In(tpe / "component" / "instrument", _)),
            types.map(In(tpe / "component" / "componentType", _))
          ).flatten
        }
      case _ =>
        Result.internalError("Unexpected arguments for 'instrumentComponentAvailability'.")

}
