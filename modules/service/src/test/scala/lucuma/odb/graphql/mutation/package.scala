// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import lucuma.core.model.TelluricType

trait TelluricTypeGraphQLFormat:
  def telluricTypeToGraphQL(tt: TelluricType): String = tt match
    case TelluricType.Hot   => "{ tag: HOT }"
    case TelluricType.A0V   => "{ tag: A0V }"
    case TelluricType.Solar => "{ tag: SOLAR }"
    case TelluricType.Manual(starTypes) =>
      val types = starTypes.toList.map(s => s""""$s"""").mkString("[", ", ", "]")
      s"{ tag: MANUAL, starTypes: $types }"
