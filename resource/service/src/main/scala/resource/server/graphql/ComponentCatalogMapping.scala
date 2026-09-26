// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.graphql

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Predicate
import grackle.Predicate.*
import grackle.Query.*
import grackle.QueryCompiler.Elab
import grackle.skunk.SkunkMapping
import grackle.sql.Like
import lucuma.core.refined.cats.given
import lucuma.odb.data.Existence
import lucuma.odb.graphql.binding.*
import resource.model.InstrumentComponentType
import resource.model.ResourceInstrument
import resource.server.graphql.table.*

/**
 * The `components` root: the component catalog a site's records cover.
 *
 * The rows come from `v_instrument_component_at_site`, so every filter is a predicate the database
 * applies. The mapping is bound to the `components` path, because the same GraphQL type is also
 * reached through a block, where it comes from the base table instead.
 */
trait ComponentCatalogMapping[F[_]] extends InstrumentComponentTables[F]:
  this: SkunkMapping[F] =>

  /** Input binding by raw tag: the tags are the GraphQL values by convention. */
  protected val ResourceInstrumentBinding: Matcher[ResourceInstrument] =
    enumeratedBinding

  protected val InstrumentComponentTypeBinding: Matcher[InstrumentComponentType] =
    enumeratedBinding

  lazy val ComponentCatalogMappings: List[TypeMapping] =
    List(
      ObjectMapping(QueryType / "components")(
        SqlField("id", InstrumentComponentAtSiteView.Id, key = true),
        SqlField("_site", InstrumentComponentAtSiteView.Site, key = true, hidden = true),
        SqlField("_search", InstrumentComponentAtSiteView.Search, hidden = true),
        SqlField("instrument", InstrumentComponentAtSiteView.Instrument),
        SqlField("componentType", InstrumentComponentAtSiteView.ComponentType),
        SqlField("code", InstrumentComponentAtSiteView.Code),
        SqlField("name", InstrumentComponentAtSiteView.Name),
        SqlField("barcode", InstrumentComponentAtSiteView.Barcode),
        SqlField("aliases", InstrumentComponentAtSiteView.Aliases),
        SqlField("existence", InstrumentComponentAtSiteView.Existence)
      )
    )

  /**
   * Turns a user's search string into a LIKE pattern. The three characters LIKE gives a meaning to
   * are escaped, so a search for `a_b` matches `a_b` and not `axb`.
   */
  private def searchPattern(s: NonEmptyString): String =
    val escaped = s.value.foldLeft(new StringBuilder) { (b, c) =>
      if c == '\\' || c == '%' || c == '_' then b += '\\'
      b += c
    }
    s"%$escaped%"

  lazy val ComponentsElaborator: ElaboratorPF =
    case (QueryType,
          "components",
          List(
            SiteBinding("site", rSite),
            ResourceInstrumentBinding.List.Option("instruments", rInsts),
            InstrumentComponentTypeBinding.List.Option("componentTypes", rTypes),
            NonEmptyStringBinding.Option("search", rSearch),
            BooleanBinding("includeDeleted", rIncl)
          )
        ) =>
      val tpe = InstrumentComponentType
      Elab
        .liftR((rSite, rInsts, rTypes, rSearch, rIncl).parTupled)
        .flatMap { (site, insts, types, search, includeDeleted) =>
          Elab.transformChild { child =>
            FilterOrderByOffsetLimit(
              pred = Some(
                List(
                  Eql(tpe / "_site", Const(site)).some,
                  insts.map(is => In(tpe / "instrument", is)),
                  types.map(ts => In(tpe / "componentType", ts)),
                  search.map(s => Like(tpe / "_search", searchPattern(s), false)),
                  Option.unless(includeDeleted)(
                    Eql(tpe / "existence", Const[Existence](Existence.Present))
                  )
                ).flatten.foldLeft[Predicate](True)(And.apply)
              ),
              oss = Some(
                List(
                  OrderSelection[ResourceInstrument](tpe / "instrument"),
                  OrderSelection[InstrumentComponentType](tpe / "componentType"),
                  OrderSelection[NonEmptyString](tpe / "code")
                )
              ),
              offset = None,
              limit = None,
              child = child
            )
          }
        }
