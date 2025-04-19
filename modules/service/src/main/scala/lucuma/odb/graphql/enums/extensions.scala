// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.enums

import cats.syntax.option.*
import grackle.EnumType
import grackle.EnumValueDefinition
import lucuma.core.util.Enumerated

extension [A](e: Enumerated[A]) {

  /**
   * Creates a Grackle `EnumType` from an `Enumerated` instance.
   *
   * @param typeName    type name for the `EnumType` instance
   * @param description description of the enumeration
   * @param valueName   function to obtain the name of a value
   */
  def toEnumType(typeName: String, description: String)(valueName: A => String): EnumType =
    EnumType(
      typeName,
      description.some,
      e.all.map { v => EnumValueDefinition(e.tag(v).toUpperCase(), valueName(v).some, Nil) },
      Nil
    )

}
