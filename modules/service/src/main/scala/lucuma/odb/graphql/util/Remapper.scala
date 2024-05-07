// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.util

import grackle.DirectiveDef
import grackle.EnumType
import grackle.Field
import grackle.InputObjectType
import grackle.InputValue
import grackle.InterfaceType
import grackle.ListType
import grackle.NamedType
import grackle.NullableType
import grackle.ObjectType
import grackle.ScalarType
import grackle.Schema
import grackle.SchemaExtension
import grackle.Type
import grackle.TypeExtension
import grackle.TypeRef
import grackle.UnionType
import org.tpolecat.sourcepos.SourcePos

object Remapper {

  /** Fix any TypeRefs that are pointing at other schemas. */
  def remap(s: Schema): Schema =
    new Schema {
      val pos: SourcePos = s.pos
      val baseTypes: List[NamedType] = s.baseTypes.map(new Remapper(this).remapNamedType)
      val directives: List[DirectiveDef] = s.directives
      def schemaExtensions: List[SchemaExtension] = 
        s.schemaExtensions match
          case Nil => Nil
          case _   => sys.error("Unimplemented: remap for schema extensions")
      def typeExtensions: List[TypeExtension] = 
        s.typeExtensions match
          case Nil => Nil
          case _   => sys.error("Unimplemented: remap for type extensions")
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
    case r @ TypeRef(_, name)                                => r.withSchema(s)
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