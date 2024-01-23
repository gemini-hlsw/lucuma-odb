// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import lucuma.odb.data.ProposalReference

val ProposalReferenceBinding: Matcher[ProposalReference] =
  StringBinding.emap { s =>
    ProposalReference
      .FromString
      .getOption(s)
      .orElse(ProposalReference.FromShortString.getOption(s))
      .toRight(s"'$s' cannot be parsed as a valid ProposalReference.")
  }
