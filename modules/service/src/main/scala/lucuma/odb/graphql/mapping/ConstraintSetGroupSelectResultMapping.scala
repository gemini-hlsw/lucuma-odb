// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping

trait ConstraintSetGroupSelectResultMapping[F[_]] extends ResultMapping[F] {

  lazy val ConstraintSetGroupSelectResultMapping: ObjectMapping =
    topLevelSelectResultMapping(ConstraintSetGroupSelectResultType)

}
