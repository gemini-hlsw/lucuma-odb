// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import edu.gemini.grackle.Value.IDValue

val IDBinding: Matcher[String] =
  primitiveBinding("ID") { case IDValue(value) => value }
