// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.util

import edu.gemini.grackle.DirectiveDef
import edu.gemini.grackle.EnumType
import edu.gemini.grackle.Field
import edu.gemini.grackle.InputObjectType
import edu.gemini.grackle.InputValue
import edu.gemini.grackle.InterfaceType
import edu.gemini.grackle.ListType
import edu.gemini.grackle.NamedType
import edu.gemini.grackle.NullableType
import edu.gemini.grackle.ObjectType
import edu.gemini.grackle.ScalarType
import edu.gemini.grackle.Schema
import edu.gemini.grackle.Type
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.UnionType
import org.tpolecat.sourcepos.SourcePos

object Remapper {

  /** Fix any TypeRefs that are pointing at other schemas. */
  def remap(s: Schema): Schema =
    new Schema {
      val pos: SourcePos = s.pos
      val types: List[NamedType] = s.types.map(new Remapper(this).remapNamedType)
      val directives: List[DirectiveDef] = s.directives
    }

}

private class Remapper(s: Schema) {

  def remapInputValue: InputValue => InputValue = {
    case InputValue(name, desc, tpe, defaultValue, dirs) =>
      InputValue(name, desc, remapType(tpe), defaultValue, dirs)
  }

  def remapField: Field => Field = {
    case Field(name, desc, args, tpe, dirs) =>
      Field(name, desc, args.map(remapInputValue), remapType(tpe), dirs)
  }

  def remapNamedType: NamedType => NamedType = {
    case TypeRef(_, name)                                    => TypeRef(s, name)
    case ScalarType(name, desc, dirs)                        => ScalarType(name, desc, dirs)
    case UnionType(name, desc, members, dirs)                => UnionType(name, desc, members.map(remapNamedType), dirs)
    case EnumType(name, desc, enumValues, dirs)              => EnumType(name, desc, enumValues, dirs)
    case InputObjectType(name, desc, inputFields, dirs)      => InputObjectType(name, desc, inputFields.map(remapInputValue), dirs)
    case InterfaceType(name, desc, fields, interfaces, dirs) => InterfaceType(name, desc, fields.map(remapField), interfaces.map(remapNamedType), dirs)
    case ObjectType(name, desc, fields, interfaces, dirs)    => ObjectType(name, desc, fields.map(remapField), interfaces.map(remapNamedType), dirs)
  }

  def remapType: Type => Type = {
    case nt: NamedType        => remapNamedType(nt)
    case ListType(ofType)     => ListType(remapType(ofType))
    case NullableType(ofType) => NullableType(remapType(ofType))
  }

}