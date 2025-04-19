// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import grackle.Value
import grackle.Value.ListValue

val ListBinding: Matcher[List[Value]] =
  primitiveBinding("List") { case ListValue(elems) => elems }
