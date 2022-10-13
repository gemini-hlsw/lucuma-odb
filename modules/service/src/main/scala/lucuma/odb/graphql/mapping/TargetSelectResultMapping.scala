// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import lucuma.odb.graphql.BaseMapping
import skunk.codec.numeric.int8
import edu.gemini.grackle.Result
import edu.gemini.grackle.Cursor
import scala.tools.util.PathResolver.Environment

object TargetSelectResultMapping {

  // keys for things we expect to find in the environment
  val LimitKey = "targetLimit"
  val AliasKey = "targetAlias"

}

// For some reason we can't put this inside the body
import TargetSelectResultMapping._

trait TargetSelectResultMapping[F[_]] extends BaseMapping[F] {

  private object root extends RootDef {
    val unused = col("<unused fake root column>", int8)
  }

  lazy val TargetSelectResultMapping: ObjectMapping =
    ObjectMapping(
      tpe = TargetSelectResultType,
      fieldMappings = List(
        SqlObject("matches"),
        CursorField("hasMore", hasMore),
        SqlField("<unused but required>", root.unused, hidden = true)
      )
    )

  def hasMore(c: Cursor): Result[Boolean] =
    for
      limit <- c.envR[Int](LimitKey)
      alias <- c.envR[Option[String]](AliasKey)
      items <- c.field("matches", alias)
      size  <- items.listSize
    yield limit < 0 || size > limit

}