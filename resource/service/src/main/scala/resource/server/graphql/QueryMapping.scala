// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.graphql

import cats.effect.Sync
import cats.syntax.all.*
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import lucuma.core.enums.Site
import lucuma.odb.graphql.binding.*
import resource.model.*
import resource.server.DummyData

import java.time.LocalDate

trait QueryMapping[F[_]: Sync] extends BaseMapping[F] {

  private val SiteParam           = "site"
  private val ObservingNightParam = "observingNight"

  val QueryType: TypeRef = schema.ref("Query")

  lazy val QueryMapping: ObjectMapping =
    ObjectMapping(QueryType)(
      RootEffect.computeEncodable("telescopeNightTimeline")(telescopeNightTimeline)
    )

  lazy val QueryElaborator: ElaboratorPF = List(
    TelescopeNightTimelineE
  ).combineAll

  val telescopeNightTimeline: ComputeF[Option[TelescopeNightTimeline]] = (_, env) =>
    (env.getR[Site](SiteParam), env.getR[LocalDate](ObservingNightParam))
      .mapN: (site, observingNight) =>
        DummyData.mockTelescopeNightTimelines
          .find(t => t.site === site && t.observingNight.equals(observingNight))
      .pure[F]

  private lazy val TelescopeNightTimelineE: ElaboratorPF =
    case (QueryType,
          "telescopeNightTimeline",
          List(
            SiteBinding(SiteParam, rSite),
            DateBinding(ObservingNightParam, rObservingNight)
          )
        ) =>
      Elab
        .liftR((rSite, rObservingNight).parTupled)
        .flatMap: (site, observingNight) =>
          Elab.env(
            SiteParam           -> site,
            ObservingNightParam -> observingNight
          )

}
