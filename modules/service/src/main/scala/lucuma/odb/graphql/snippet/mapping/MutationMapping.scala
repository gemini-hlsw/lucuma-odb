// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package mapping

import mutation._

import edu.gemini.grackle.skunk.SkunkMapping
import com.sourcegraph.semanticdb_javac.Semanticdb.TypeRef
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query.Select
import edu.gemini.grackle.Result

trait MutationMapping[F[_]]
  extends CreateProgramMutation[F]
     with UpdateProgramsMutation[F]
  { this: SkunkMapping[F] =>

  lazy val MutationType = schema.ref("Mutation")

  lazy val MutationMapping =
    ObjectMapping(
      tpe = MutationType,
      fieldMappings = List(
        SqlRoot("createProgram", mutation = CreateProgramMutation),
        SqlRoot("updatePrograms", mutation = UpdateProgramsMutation),
      //   SqlRoot("linkUser", mutation = linkUser),
      )
    )

}