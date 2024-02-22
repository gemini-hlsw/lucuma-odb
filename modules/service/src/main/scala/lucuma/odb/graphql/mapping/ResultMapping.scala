// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import cats.syntax.all._
import grackle.Cursor
import grackle.Cursor.ListTransformCursor
import grackle.Env
import grackle.Query
import grackle.Query.*
import grackle.Result
import grackle.Type
import io.circe.Encoder
import lucuma.odb.graphql.BaseMapping
import skunk.codec.numeric.int8

object ResultMapping {

  /** Maximum (and default) page limit. */
  val MaxLimit = 1000

  // keys for things we expect to find in the environment
  val LimitKey = "resultLimit"
  val AliasKey = "resultAlias"

  private def hasMore(collectionField: String)(c: Cursor): Result[Boolean] =
    for
      limit <- c.envR[Int](LimitKey)
      alias <- c.envR[Option[String]](AliasKey)
      items <- c.field(collectionField, alias)
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
    def mapSomeFields(query: Query)(f: PartialFunction[Query, Query]): Query =
      self.mapFields(query) { q =>
        f.lift(q) match
          case None => q
          case Some(qq) => qq
      } 

  private def result(child: Query, limit: Int, collectionField: String)(transform: Query => Query): Result[Query] = {

    // Find the "matches" node under the main "targets" query and add all our filtering
    // and whatnot down in there, wrapping with a transform that removes the last row from the
    // final results. See an `SelectResultMapping` to see how `hasMore` works.
    def transformMatches(q: Query): Query =
      Query.mapSomeFields(q) {
        case Select(`collectionField`, alias, child) =>
          Select(collectionField, alias, TransformCursor(Take(limit), transform(child)))
      }

    // If we're selecting collectionField then continue by transforming the child query, otherwise
    // punt because there's really no point in doing such a selection.
    if !Query.hasField(child, collectionField)
    then Result.internalError(s"Field `$collectionField` must be selected.") // meh
    else
      Result {
        Environment(
          Env(
            ResultMapping.LimitKey -> limit,
            ResultMapping.AliasKey -> Query.fieldAlias(child, collectionField),
          ),
          transformMatches(child)
        )
      }

  }

  def selectResult(child: Query, limit: Int)(transform: Query => Query): Result[Query] =
    result(child, limit, "matches")(transform)

  def mutationResult(child: Query, limit: Int, collectionField: String)(transform: Query => Query): Result[Query] =
    result(child, limit, collectionField)(transform)

}

trait ResultMapping[F[_]] extends BaseMapping[F] {

  private object root extends RootDef {
    val bogus = col("<bogus root column>", int8)
  }

  private def resultMapping(tpe: Type, collectionField: String, parentKeyColumn: ColumnRef, joins: Join*): ObjectMapping =
    ObjectMapping(
      tpe = tpe,
      fieldMappings = List(
        SqlObject(collectionField, joins: _*),
        CursorField("hasMore", ResultMapping.hasMore(collectionField)),
        SqlField("<key>", parentKeyColumn, key = (parentKeyColumn ne root.bogus), hidden = true)
      )
    )

  def topLevelSelectResultMapping(tpe: Type): ObjectMapping =
    resultMapping(tpe, "matches", root.bogus)

  def nestedSelectResultMapping(tpe: Type, parentKeyColumn: ColumnRef, joins: Join*): ObjectMapping =
    resultMapping(tpe, "matches", parentKeyColumn, joins: _*)

  def updateResultMapping(tpe: Type, collectionField: String): ObjectMapping =
    resultMapping(tpe, collectionField, root.bogus)

}
