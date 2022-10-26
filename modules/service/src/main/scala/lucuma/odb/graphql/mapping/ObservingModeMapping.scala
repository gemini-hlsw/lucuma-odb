// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.syntax.functor.*
import cats.syntax.option.*
import edu.gemini.grackle.Path
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.Result
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.core.model.Target
import lucuma.odb.data.Existence
import lucuma.odb.data.ObservingModeType
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.AsterismTargetTable

import binding._
import input._
import table._

trait ObservingModeMapping[F[_]]
  extends ObservationView[F]
     with GmosNorthLongSlitTable[F] { this: SkunkMapping[F] =>

  lazy val ObservingModeMapping: ObjectMapping =
    ObjectMapping(
      tpe = ObservingModeType,
      fieldMappings = List(
        SqlField("synthetic_id", ObservationView.ObservingMode.SyntheticId, key = true, hidden = true),
          
        FieldRef[ObservingModeType]("mode").as("instrument", _.instrument),
        SqlField("mode", ObservationView.ObservingMode.ObservingModeType),

        SqlObject("gmosNorthLongSlit", Join(ObservationView.Id, GmosNorthLongSlitTable.ObservationId)),
//        SqlObject("gmosSouthLongSlit", Join(ObservationView.Id, GmosSouthLongSlitTable.ObservationId)
      )
    )

}
