// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import cats.syntax.all.*
import grackle.Query.Binding
import grackle.Result
import lucuma.odb.graphql.input.atMostOne
import lucuma.odb.graphql.input.oneOrDefault
import lucuma.odb.graphql.input.oneOrFail

/**
 * Matches an input object where exactly one of the named fields is set, and gives the value of that field.
 * 
 * This is useful for GraphQL `@oneOf` input objects.
 */
def OneOfBinding[A](fields: (String, Matcher[? <: A])*): Matcher[A] =
  optionalFields(fields).rmap(oneOrFail(_*))

/**
 * Matches an input object where at most one of the named fields is set, and gives the value of that field, or `default` if no field is set.
 */
def OneOfOrDefaultBinding[A](default: A)(fields: (String, Matcher[? <: A])*): Matcher[A] =
  optionalFields(fields).rmap(oneOrDefault(default)(_*))

/**
 * Matches an input object where at most one of the named fields is set, and gives the value of that field, or `None` if no field is set.
 */
def AtMostOneBinding[A](fields: (String, Matcher[? <: A])*): Matcher[Option[A]] =
  optionalFields(fields).rmap(atMostOne(_*))

private def optionalFields[A](
  fields: Seq[(String, Matcher[? <: A])]
): Matcher[List[(Option[A], String)]] =
  val names   = fields.map(_._1)
  val nameSet = names.toSet
  require(nameSet.size == names.size, s"Duplicate field names in ${names.mkString(", ")}")
  ObjectFieldsBinding.rmap: values =>
    val byName  = values.toMap
    val unknown = values.map(_._1).filterNot(nameSet)
    val missing = names.filterNot(byName.keySet)
    if unknown.nonEmpty then
      Result.failure(s"Unhandled field(s) ${unknown.mkString(", ")}; expected only ${names.mkString(", ")}")
    else if missing.nonEmpty then
      Result.failure(s"Missing field(s) ${missing.mkString(", ")}; expected ${names.mkString(", ")}")
    else
      fields.toList.parTraverse: (name, m) =>
        m.Option.validate(Binding(name, byName(name))).tupleRight(name)
