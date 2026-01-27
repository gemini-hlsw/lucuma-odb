// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.table

import lucuma.odb.graphql.BaseMapping
import lucuma.odb.util.Codecs.atom_id
import lucuma.odb.util.Codecs.visit_id

trait VisitAtomView[F[_]] extends BaseMapping[F]:
  object VisitAtomView extends TableDef("v_visit_atom"):
    val VisitId: ColumnRef = col("c_visit_id", visit_id)
    val AtomId:  ColumnRef = col("c_atom_id",  atom_id)