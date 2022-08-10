// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package mapping

import edu.gemini.grackle.sql.SqlMapping

trait QueryMapping[F[_]] { this: SqlMapping[F] =>

  lazy val QueryType = schema.ref("Query")

  lazy val QueryMapping =
    ObjectMapping(
      tpe = QueryType,
      fieldMappings = List(
        SqlRoot("program"),
        SqlRoot("programs"),
      )
    )

}