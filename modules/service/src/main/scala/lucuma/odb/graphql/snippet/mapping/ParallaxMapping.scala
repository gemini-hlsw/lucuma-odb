// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package mapping

import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.core.math.Parallax
import lucuma.odb.graphql.util.MappingExtras

import java.math.MathContext

import table.TargetView
import table.ProgramTable

trait ParallaxMapping[F[_]]
  extends ProgramTable[F]
     with TargetView[F]
     with MappingExtras[F] { this: SkunkMapping[F] =>

  lazy val ParallaxType = schema.ref("Parallax")

  lazy val ParallaxMapping =
    ObjectMapping(
      tpe = ParallaxType,
      fieldMappings = List(
        SqlField("synthetic_id", TargetView.Sidereal.Parallax.SyntheticId, key = true, hidden = true),
        SqlField("value", TargetView.Sidereal.Parallax.Value, hidden = true),
        FieldRef[Parallax]("value").as("microarcseconds", _.μas.value.value),
        FieldRef[Parallax]("value").as("milliarcseconds", a => a.mas.value.toBigDecimal(MathContext.DECIMAL128)),
      )
    )

  }

