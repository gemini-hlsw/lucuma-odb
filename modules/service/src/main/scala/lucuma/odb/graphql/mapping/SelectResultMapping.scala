// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import cats.syntax.all._
import edu.gemini.grackle.Cursor
import edu.gemini.grackle.Cursor.Env
import edu.gemini.grackle.Cursor.ListTransformCursor
import edu.gemini.grackle.Mapping
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query.*
import edu.gemini.grackle.Result
import edu.gemini.grackle.Type
import io.circe.Encoder
import lucuma.odb.graphql.BaseMapping
import lucuma.odb.graphql.mapping.SelectResultMapping
import skunk.codec.numeric.int8

object SelectResultMapping {

  // keys for things we expect to find in the environment
  val LimitKey = "selectResultLimit"
  val AliasKey = "selectResultAlias"

  private def hasMore(c: Cursor): Result[Boolean] =
    for
      limit <- c.envR[Int](LimitKey)
      alias <- c.envR[Option[String]](AliasKey)
      items <- c.field("matches", alias)
      size  <- items.listSize
    yield limit < 0 || size > limit


  /** A cursor transformation that takes `n` elements from a list (if it's a list). */
  object Take {
    def apply(n: Int)(c: Cursor): Result[Cursor] =
      (c.listSize, c.asList(Seq)).mapN { (size, elems) =>
        if size <= n then c
        else ListTransformCursor(c, size - 1, elems.init)
      }
  }
 
  extension (self: Query.type)
    def mapSomeFields(query: Query)(f: PartialFunction[Query, Result[Query]]): Result[Query] = 
      self.mapFields(query)(f.applyOrElse(_, Result.apply))

  /**
   * Transform a top-level `Select(field, ..., child)` that yields a SelectResult with a specified
   * `limit` into the proper form. The "matches" subselect's child is passed to `transform`, which
   * is how you add filter, ordering, limit (MUST BE `limit + 1`!) and so on as if it were a
   * top-level query without the SelectResult structure. See `TargetMapping` for instance, for an 
   * example. Note that this will fail if there is no "matches" subquery. Supporting such queries
   * would complicate things and isn't really necessary.
   */
  def selectResult(field: String, child: Query, limit: Int)(transform: Query => Query): Result[Query] = {

    // Find the "matches" node under the main "targets" query and add all our filtering
    // and whatnot down in there, wrapping with a transform that removes the last row from the
    // final results. See an `SelectResultMapping` to see how `hasMore` works.
    def transformMatches(q: Query): Result[Query] =
      Query.mapSomeFields(q) {
        case Select("matches", Nil, child) =>
          Result(Select("matches", Nil, TransformCursor(Take(limit), transform(child))))
      }

    // If we're selecting "matches" then continue by transforming the child query, otherwise
    // punt because there's really no point in doing such a selection.
    if !Query.hasField(child, "matches") 
    then Result.failure("Field `matches` must be selected.") // meh
    else
      transformMatches(child).map { child =>
        Select(field, Nil,
          Environment(
            Env(
              SelectResultMapping.LimitKey -> limit,
              SelectResultMapping.AliasKey -> Query.fieldAlias(child, "matches"),
            ),
            child
          )
        )
      }

  }

}

trait SelectResultMapping[F[_]] extends BaseMapping[F] {

  private object root extends RootDef {
    val bogus = col("<bogus root column>", int8)
  }

  def topLevelSelectResultMapping(tpe: Type): ObjectMapping =
    ObjectMapping(
      tpe = tpe,
      fieldMappings = List(
        SqlObject("matches"),
        CursorField("hasMore", SelectResultMapping.hasMore),
        SqlField("<key>", root.bogus, hidden = true) // n.b. no key = true here
      )
    )
  def nestedSelectResultMapping(tpe: Type, parentKeyColumn: ColumnRef, joins: Join*): ObjectMapping =
    ObjectMapping(
      tpe = tpe,
      fieldMappings = List(
        SqlObject("matches", joins: _*),
        CursorField("hasMore", SelectResultMapping.hasMore),
        SqlField("<key>", parentKeyColumn, key = true, hidden = true)
      )
    )

}