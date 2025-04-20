// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import lucuma.core.model.ObservationReference

val ObservationReferenceBinding: Matcher[ObservationReference] =
  StringBinding.emap { s =>
    ObservationReference
      .fromString
      .getOption(s)
      .toRight(s"'$s' cannot be parsed as a valid ObservationReference.")
  }
