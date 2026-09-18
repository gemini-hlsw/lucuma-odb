// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.graphql

import cats.effect.Async
import cats.effect.Resource
import cats.syntax.all.*
import grackle.Cursor
import grackle.Env
import grackle.Path
import grackle.Query
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.ResultT
import grackle.skunk.SkunkMapping
import io.circe.Json
import lucuma.core.enums.Site
import lucuma.core.model.ObservingNight
import lucuma.odb.graphql.binding.*
import resource.server.NightProjection
import skunk.Session

import java.time.LocalDate
import java.time.temporal.ChronoUnit

trait TelescopeNightMapping[F[_]: Async] extends BaseMapping[F]:
  this: SkunkMapping[F] =>

  def nightPool: Resource[F, Session[F]]

  protected val NightSiteKey   = "nightSite"
  protected val NightsStartKey = "nightsStart"
  protected val NightsEndKey   = "nightsEnd"

  /**
   * The nights of [start, end) as JSON. Both roots come through here; `telescopeNight` asks for the
   * one night that starts on its date.
   *
   * The client's query decides which block tables are read, because a night that selects only
   * `dataAvailable` needs no block columns at all.
   */
  private def nightsJson(query: Query, env: Env): F[Result[List[Json]]] =
    (for
      st    <- ResultT.fromResult(env.getR[Site](NightSiteKey))
      start <- ResultT.fromResult(env.getR[LocalDate](NightsStartKey))
      end   <- ResultT.fromResult(env.getR[LocalDate](NightsEndKey))
      nights = LazyList
                 .iterate(ObservingNight.fromSiteAndLocalDate(st, start))(_.next)
                 .takeWhile(_.toLocalDate.isBefore(end))
                 .toList
      spans <- ResultT.fromResult(nights.traverse(NightProjection.nightSpan))
      sel    = NightProjection.Selection.fromQuery(query)
      json  <- ResultT.liftF(nightPool.use { s =>
                 NightProjection
                   .allRows(s, st, spans.head.start, spans.last.end, sel)
                   .map(rows =>
                     nights.zip(spans).map { (n, span) =>
                       NightProjection.nightJson(st, n, span, rows, sel)
                     }
                   )
               })
    yield json).value

  /**
   * The child query of a root, which is the selection over one TelescopeNight in both roots.
   * `telescopeNights` wraps that selection in a list, which does not change the fields.
   */
  private def nightSelection(query: Query): Query =
    Query.extractChild(query).getOrElse(query)

  val telescopeNightHandler: (Query, Path, Env) => F[Result[(Query, Cursor)]] =
    (query, path, env) =>
      nightsJson(nightSelection(query), env).map(
        _.flatMap {
          case List(json) => Result((query, circeCursor(path, env, json)))
          case other      =>
            Result.internalError(s"Expected exactly one night, found ${other.length}.")
        }
      )

  val telescopeNightsHandler: (Query, Path, Env) => F[Result[(Query, Cursor)]] =
    (query, path, env) =>
      nightsJson(nightSelection(query), env).map(
        _.map(json => (query, circeCursor(path, env, Json.arr(json*))))
      )

  lazy val TelescopeNightElaborator: ElaboratorPF =
    case (QueryType,
          "telescopeNight",
          List(
            SiteBinding("site", rSite),
            DateBinding("observingNight", rDate)
          )
        ) =>
      Elab.liftR((rSite, rDate).parTupled).flatMap { (st, date) =>
        Elab.env(NightSiteKey -> st, NightsStartKey -> date, NightsEndKey -> date.plusDays(1))
      }
    case (QueryType,
          "telescopeNights",
          List(
            SiteBinding("site", rSite),
            DateBinding("start", rStart),
            DateBinding("end", rEnd)
          )
        ) =>
      for
        args            <- Elab.liftR((rSite, rStart, rEnd).parTupled)
        (st, start, end) = args
        _               <- Elab.liftR(
                             validateWindow(start.isBefore(end), ChronoUnit.DAYS.between(start, end), "nights")
                           )
        _               <- Elab.env(NightSiteKey -> st, NightsStartKey -> start, NightsEndKey -> end)
      yield ()
