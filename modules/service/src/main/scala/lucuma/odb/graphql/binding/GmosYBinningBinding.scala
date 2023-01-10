// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import lucuma.core.enums.GmosYBinning

val GmosYBinningBinding: Matcher[GmosYBinning] =
  enumeratedBinding
