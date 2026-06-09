// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.graphql

import cats.syntax.all.*
import grackle.Predicate.*
import grackle.Query.*
import grackle.QueryCompiler.Elab
import grackle.skunk.SkunkMapping
import lucuma.core.enums.Site
import lucuma.odb.graphql.binding.*
import org.typelevel.cats.time.*
import resource.server.graphql.table.*

trait QueryMapping[F[_]] extends TelescopeNightTimelineTable[F] {

  private val SiteParam           = "site"
  private val ObservingNightParam = "observingNight"

  lazy val QueryMapping: ObjectMapping =
    ObjectMapping(QueryType)(
      SqlObject("telescopeNightTimeline")
    )

  lazy val QueryElaborator: ElaboratorPF = List(
    TelescopeNightTimelineE
  ).combineAll

  private lazy val TelescopeNightTimelineE: ElaboratorPF =
    case (QueryType, "telescopeNightTimeline", args) =>
      val rSite           = SiteBinding.unapply(args.find(_.name == SiteParam).get) match {
        case Some((_, r)) => r
      }
      val rObservingNight =
        DateBinding.unapply(args.find(_.name == ObservingNightParam).get) match {
          case Some((_, r)) => r
        }
      Elab.transformChild { child =>
        rSite.flatMap { site =>
          rObservingNight.map { observingNight =>
            Unique(
              Filter(
                And(
                  Eql(TelescopeNightTimelineType / "site", Const(site)),
                  Eql(TelescopeNightTimelineType / "observingNight", Const(observingNight))
                ),
                child
              )
            )
          }
        }
      }

}
