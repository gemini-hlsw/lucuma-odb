// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import edu.gemini.grackle.EnumValue
import edu.gemini.grackle.Value.TypedEnumValue

val TypedEnumBinding: Matcher[EnumValue] =
  primitiveBinding("TypedEnum") { case TypedEnumValue(value) => value }
