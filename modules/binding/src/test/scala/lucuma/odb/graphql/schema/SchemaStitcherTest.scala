// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.schema

import fs2.io.file.Path
import grackle.Result
import grackle.Result.Success
import grackle.Schema
import munit.FunSuite

class SchemaStitcherTest extends FunSuite {

  import SchemaStitcherTest.*

  test("SchemaStitcher should parse import statements") {
    val elements = schemaResolver
      .resolve(Path("baseSchema.graphql"))
      .map(SchemaStitcher.importLineParser.parse)
      .collect { case Right((_, (els, path))) =>
        (els, path)
      }
    assertEquals(elements.length, 2)
    elements(0) match {
      case (SchemaStitcher.AllElements, path) => assertEquals(path, Path("schema1.graphql"))
      case other                              => fail(s"Unexpected parse result: $other")
    }
    elements(1) match {
      case (SchemaStitcher.ElementList(els), path) =>
        assertEquals(els.toNonEmptyList.toList.map(_.value), List("TypeA", "TypeX"))
        assertEquals(path, Path("schema2.graphql"))
      case other                                   => fail(s"Unexpected parse result: $other")
    }
  }

  test("SchemaStitcher should compose schema") {
    val result = SchemaStitcher(Path("baseSchema.graphql"), schemaResolver).build
    (result, expectedSchema) match {
      case (Success(a), Success(b)) => assertEquals(a.toString, b.toString)
      case _                        => fail("Error creating schema")
    }
  }

  test("SchemaStitcher should find imports on indented lines") {
    val src    = SchemaSource.fromStringMap(
      Map(
        Path("indented.graphql") -> """
          |  #import TypeA from "schema2.graphql"
          |
          |type Query {
          |  query1: TypeA!
          |}
        """.stripMargin,
        Path("schema2.graphql")  -> schema2
      )
    )
    val result = SchemaStitcher(Path("indented.graphql"), src).build
    result match {
      case Success(s) => assert(s.types.exists(_.name == "TypeA"), "indented import was not resolved")
      case other      => fail(s"Error creating schema: $other")
    }
  }

  test("SchemaStitcher should build a schema with no imports") {
    val result = SchemaStitcher(Path("noImportSchema.graphql"), schemaResolver).build
    (result, Schema(noImportSchema)) match {
      case (Success(a), Success(b)) => assertEquals(a.toString, b.toString)
      case other                    => fail(s"Error creating schema: $other")
    }
  }

  test("SchemaStitcher should keep unreferenced types when there are no imports") {
    SchemaStitcher(Path("noImportSchema.graphql"), schemaResolver).build match {
      case Success(s) =>
        assert(s.types.exists(_.name == "Unreferenced"), "unreferenced type was dropped")
        assertEquals(
          s.types.find(_.name == "TypeD").flatMap(_.description),
          Some("A described type.")
        )
      case other      => fail(s"Error creating schema: $other")
    }
  }

}

object SchemaStitcherTest {

  val schema1: String = """
    |#import TypeA from "schema2.graphql"
    |
    |type TypeB {
    |  val1: TypeA!
    |}
  """.stripMargin

  val schema2: String = """
    |enum EnumX {
    |  VAL0
    |  VAL1
    |}
    |
    |type TypeA {
    |  attr0: [EnumX]!
    |}
    |
    |type TypeB {
    |  attr0: Boolean!
    |  attr1: Float
    |}
    |
    |type TypeX {
    |  attr0: [TypeA]!
    |}
  """.stripMargin

  val noImportSchema: String = """
    |"A described type."
    |type TypeD {
    |  attr0: Int!
    |}
    |
    |type Unreferenced {
    |  attr0: Boolean!
    |}
    |
    |type Query {
    |  query1: TypeD!
    |}
  """.stripMargin

  val baseSchema: String = """
    |#import * from "schema1.graphql"
    |#import TypeA, TypeX from "schema2.graphql"
    |
    |type TypeC {
    |  attr0: TypeB
    |}
    |
    |type Query {
    |  query1(par: TypeA!): TypeC!
    |  query2(par: TypeX!): TypeC!
    |}
  """.stripMargin

  val expectedSchema: Result[Schema] = Schema("""
    |enum EnumX {
    |  VAL0
    |  VAL1
    |}
    |
    |type TypeA {
    |  attr0: [EnumX]!
    |}
    |
    |type TypeX {
    |  attr0: [TypeA]!
    |}
    |
    |type TypeB {
    |  val1: TypeA!
    |}
    |
    |type TypeC {
    |  attr0: TypeB
    |}
    |
    |type Query {
    |  query1(par: TypeA!): TypeC!
    |  query2(par: TypeX!): TypeC!
    |}
    |""".stripMargin)

  val schemaResolver: SchemaSource = SchemaSource.fromStringMap(
    Map(
      Path("baseSchema.graphql")     -> baseSchema,
      Path("noImportSchema.graphql") -> noImportSchema,
      Path("schema1.graphql")        -> schema1,
      Path("schema2.graphql")        -> schema2
    )
  )

}
