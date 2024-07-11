// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.skunk.SkunkMapping
import skunk.codec.numeric.int8

trait SetAllocationsResultMapping[F[_]] extends BaseMapping[F] {

  private object root extends RootDef {
    val bogus = col("<bogus root column>", int8)
  }

  // ResultMapping.resultMapping without the "hasMore" field, which is not
  // useful in this case because at most there are count(partner) * count(band)
  // allocations.  Typically there will be fewer than 2 or 3 or so, and most
  // commonly just 1.
  lazy val SetAllocationsResultMapping: ObjectMapping =
    ObjectMapping(SetAllocationsResultType)(
      SqlObject("allocations"),
      SqlField("<key>", root.bogus, key = false, hidden = true)
    )

}

