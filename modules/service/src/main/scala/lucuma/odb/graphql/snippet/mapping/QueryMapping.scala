// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package mapping

import table._

import edu.gemini.grackle.sql.SqlMapping
import com.sourcegraph.semanticdb_javac.Semanticdb.TypeRef
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query.Select
import edu.gemini.grackle.Result

trait QueryMapping[F[_]] { this: SqlMapping[F] =>

  lazy val QueryType = schema.ref("Query")

  lazy val QueryMapping =
    ObjectMapping(
      tpe = QueryType,
      fieldMappings = List(
        SqlRoot("programs"),
      )
    )

}