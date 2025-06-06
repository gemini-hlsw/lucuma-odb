// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import grackle.Matcher
import lucuma.core.enums.{GmosXBinning, GmosYBinning}

// Binding for the unified GmosBinning GraphQL enum that can match to either GmosXBinning or GmosYBinning
object GmosBinningBinding {
  
  // Converts GraphQL GmosBinning to GmosXBinning
  val ToX: Matcher[GmosXBinning] =
    EnumBinding.map(_.toUpperCase).emap {
      case "ONE"  => Right(GmosXBinning.One)
      case "TWO"  => Right(GmosXBinning.Two)
      case "FOUR" => Right(GmosXBinning.Four)
      case other  => Left(s"Invalid GmosBinning value: $other")
    }
  
  // Converts GraphQL GmosBinning to GmosYBinning
  val ToY: Matcher[GmosYBinning] =
    EnumBinding.map(_.toUpperCase).emap {
      case "ONE"  => Right(GmosYBinning.One)
      case "TWO"  => Right(GmosYBinning.Two)
      case "FOUR" => Right(GmosYBinning.Four)
      case other  => Left(s"Invalid GmosBinning value: $other")
    }
}