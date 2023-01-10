// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import edu.gemini.grackle.Value

val ObjectFieldsBinding: Matcher[List[(String, Value)]] =
  ObjectBinding.map(_.fields)
