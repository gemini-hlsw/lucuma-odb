// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import lucuma.core.data.EmailAddress

val EmailAddressBinding: Matcher[EmailAddress] =
  StringBinding.emap: s =>
    EmailAddress.From.getOption(s).toRight(s"Invalid email address: $s")
