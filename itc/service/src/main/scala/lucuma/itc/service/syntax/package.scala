// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.syntax

import cats.Semigroup
import io.circe.ACursor

object all extends ItcSyntax with ItcGraphSyntax:

  // An "orElse" semigroup for ACursor
  given Semigroup[ACursor] =
    new Semigroup[ACursor]:
      def combine(a: ACursor, b: ACursor): ACursor =
        if (a.failed) b else a
