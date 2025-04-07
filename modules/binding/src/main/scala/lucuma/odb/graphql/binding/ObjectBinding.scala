// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import grackle.Value.ObjectValue

val ObjectBinding: Matcher[ObjectValue] =
  primitiveBinding("Input") { case ov: ObjectValue => ov }
