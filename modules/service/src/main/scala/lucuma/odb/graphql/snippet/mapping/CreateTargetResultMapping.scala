// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package mapping

import edu.gemini.grackle.skunk.SkunkMapping

import table.TargetView
import table.ProgramTable

trait CreateTargetResultMapping[F[_]]
  extends ProgramTable[F]
     with TargetView[F] { this: SkunkMapping[F] =>

  lazy val CreateTargetResultType = schema.ref("CreateTargetResult")

  lazy val CreateTargetResultMapping =
    ObjectMapping(
      tpe = CreateTargetResultType,
      fieldMappings = List(
        SqlField("id", TargetView.TargetId, key = true),
        SqlObject("target"),
      ),
    )

  }

