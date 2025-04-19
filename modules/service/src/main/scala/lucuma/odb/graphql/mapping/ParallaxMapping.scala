// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping
import lucuma.core.math.Parallax

import java.math.MathContext

import table.TargetView
import table.ProgramTable

trait ParallaxMapping[F[_]] extends ProgramTable[F] with TargetView[F] {

  lazy val ParallaxMapping =
    ObjectMapping(ParallaxType)(
      SqlField("synthetic_id", TargetView.Sidereal.Parallax.SyntheticId, key = true, hidden = true),
      SqlField("value", TargetView.Sidereal.Parallax.Value, hidden = true),
      FieldRef[Parallax]("value").as("microarcseconds", _.μas.value.value),
      FieldRef[Parallax]("value").as("milliarcseconds", a => a.mas.value.toBigDecimal(MathContext.DECIMAL128)),
    )

  }

