// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import edu.gemini.grackle.Cursor
import edu.gemini.grackle.Result
import lucuma.odb.graphql.BaseMapping
import skunk.codec.numeric.int8

import scala.tools.util.PathResolver.Environment

trait ProgramSelectResultMapping[F[_]] extends ResultMapping[F] {
  
  lazy val ProgramSelectResultMapping: ObjectMapping =
    topLevelSelectResultMapping(ProgramSelectResultType)

}