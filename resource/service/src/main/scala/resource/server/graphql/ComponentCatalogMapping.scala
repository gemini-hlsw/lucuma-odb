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
 * The InstrumentComponent type and the `components` root that lists a site's catalog.
 *
 * The `components` root reads `v_instrument_component_at_site`, one row per (site, component), so
 * that the site filter and the alias search are predicates the database applies. A block's
 * `component` field reads `t_instrument_component` directly (see ResourceBlockMappings).
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
      ObjectMapping(InstrumentComponentType)(
        (SqlField("_site", InstrumentComponentAtSiteView.Site, key = true, hidden = true) ::
          SqlField("_search", InstrumentComponentAtSiteView.Search, hidden = true) ::
          componentFields(InstrumentComponentAtSiteView))*
      )
    )

  /**
   * Turns a user's search string into an ILIKE pattern. The three characters LIKE gives a meaning
   * to are escaped, so a search for `a_b` matches `a_b` and not `axb`. The ILIKE alone makes the
   * match case-insensitive: c_search keeps the case of the stored fields.
   */
  private def searchPattern(s: NonEmptyString): String =
    s"%${s.value.replaceAll("""([\\%_])""", """\\$1""")}%"

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
                Predicate.and(
                  List(
                    Eql(tpe / "_site", Const(site)).some,
                    insts.map(is => In(tpe / "instrument", is)),
                    types.map(ts => In(tpe / "componentType", ts)),
                    search.map(s => Like(tpe / "_search", searchPattern(s), true)),
                    Option.unless(includeDeleted)(
                      Eql(tpe / "existence", Const[Existence](Existence.Present))
                    )
                  ).flatten
                )
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
