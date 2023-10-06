// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import lucuma.odb.graphql.table.*

trait DatasetSelectResultMapping[F[_]]
  extends DatasetTable[F]
     with ResultMapping[F] {

  lazy val DatasetSelectResultMapping: TypeMapping =
    SwitchMapping(
      DatasetSelectResultType,
      List(
        QueryType / "datasets" -> topLevelSelectResultMapping(DatasetSelectResultType)
      )
    )

}
