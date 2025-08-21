// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

trait DatasetChronicleEntrySelectResultMapping[F[_]] extends ResultMapping[F]:

  lazy val DatasetChronicleEntrySelectResultMapping: TypeMapping =
    topLevelSelectResultMapping(DatasetChronicleEntrySelectResultType)