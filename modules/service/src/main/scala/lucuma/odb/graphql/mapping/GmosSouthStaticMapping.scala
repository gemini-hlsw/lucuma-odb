// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.syntax.*
import io.circe.Json

import table.GmosStaticTables

trait GmosSouthStaticMapping[F[_]] extends GmosStaticTables[F]:
  lazy val GmosSouthStaticMapping: ObjectMapping =
    ObjectMapping(GmosSouthStaticType)(
      SqlField("id",            GmosSouthStaticTable.Id, key = true, hidden = true),
      SqlField("detector",      GmosSouthStaticTable.Detector),
      SqlField("mosPreImaging", GmosSouthStaticTable.MosPreImaging),
      SqlField("stageMode",     GmosSouthStaticTable.StageMode),
      CursorFieldJson("nodAndShuffle", _ => Json.Null.success, Nil)
    )