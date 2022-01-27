package lucuma.odb.graphql

import cats.Semigroup
import cats.syntax.all._
import edu.gemini.grackle.Directive
import edu.gemini.grackle.NamedType
import edu.gemini.grackle.Schema
import org.tpolecat.sourcepos.SourcePos
import edu.gemini.grackle.ObjectType
import edu.gemini.grackle.Mapping
import edu.gemini.grackle.QueryCompiler
import edu.gemini.grackle.{Query, Type}
import edu.gemini.grackle.Query.Select
import edu.gemini.grackle.Result
import edu.gemini.grackle.TypeRef

trait SnippetMapping[F[_]] extends Mapping[F] with SchemaInstances with SelectElaboratorInstances {

  case class Snippet(
    schema: Schema,
    typeMappings: List[TypeMapping],
    elaborator: Map[TypeRef, PartialFunction[Select, Result[Query]]] = Map.empty
  ) {
    def selectElaborator = new QueryCompiler.SelectElaborator(elaborator.map { case (k, v) =>
      schema.ref(k).getOrElse(sys.error(s"Elaborator references type $k, which is not present in schema:\n$schema")) -> v
    })
  }

  implicit val SemigroupObjectMapping: Semigroup[ObjectMapping] = (a, b) =>
    if (!(a.tpe.asNamed.map(_.name) == b.tpe.asNamed.map(_.name))) a
    else ObjectMapping(
      a.tpe,
      (a.fieldMappings ++ b.fieldMappings).distinctBy(_.fieldName)
    )

  implicit val SemigroupTypeMapping: Semigroup[TypeMapping] = (a, b) =>
    (a, b) match {
      case (a: ObjectMapping, b: ObjectMapping) => a |+| b
      case (a: LeafMapping[_], _: LeafMapping[_]) => a
    }


  // Rereference a type in another schema. Failing to do this is an unrecoverable error.
  def reref(s: Schema): TypeMapping => TypeMapping = {
    case om @ ObjectMapping.DefaultObjectMapping(tpe, fm) =>
      // println(s"remapping $tpe defined at ${om.pos}")
      s.ref(tpe).map(ObjectMapping(_, fm)(om.pos)).getOrElse(sys.error(s"SnippetMapping: Type ${om.tpe} doesn't exist in schema:\n$s"))
    case lm : LeafMapping.DefaultLeafMapping[_] => s.ref(lm.tpe).map(t => lm.copy(tpe = t)).getOrElse(sys.error(s"SnippetMapping: Type ${lm.tpe} doesn't exist in schema:\n$s"))
    case PrimitiveMapping(tpe) => s.ref(tpe).map(PrimitiveMapping(_)).getOrElse(sys.error(s"SnippetMapping: Type ${tpe} doesn't exist in schema:\n$s"))
    case om => sys.error(s"SnippetMapping: I don't know how to retarget a type for a ${om.getClass.getName}")
  }

  implicit val SemigroupSnippetData: Semigroup[Snippet] = (a, b) => {
    val s1 = a.schema |+| b.schema
    Snippet(
      s1,
      (a.typeMappings.map(a => b.typeMappings.find(a.tpe.asNamed.map(_.name) === _.tpe.asNamed.map(_.name)).foldLeft(a)(_ |+| _)) ++
       b.typeMappings.filterNot(b => a.typeMappings.exists(_.tpe.asNamed.map(_.name) === b.tpe.asNamed.map(_.name)))).map(reref(s1)),
      a.elaborator |+| b.elaborator,
    )
  }

  implicit def semigroupPartialFunction[A, B]: Semigroup[PartialFunction[A, B]] = (a, b) =>
    a orElse b

}

trait SelectElaboratorInstances {

  // Rereference a SelectElaborator.
  def rerefSE(s: Schema): QueryCompiler.SelectElaborator => QueryCompiler.SelectElaborator = se =>
    new QueryCompiler.SelectElaborator(Map.empty) {
      override def transform(query: Query, vars: Query.Vars, schema: Schema, tpe: Type): Result[Query] =
        s.ref(tpe).map(se.transform(query, vars, s, _)).getOrElse(sys.error(s"SnippetMapping: Type ${tpe} doesn't exist in schema:\n$s"))
      override def elaborateArgs(tpe: Type, fieldName: String, args: List[Query.Binding]): Result[List[Query.Binding]] =
        s.ref(tpe).map(se.elaborateArgs(_, fieldName, args)).getOrElse(sys.error(s"SnippetMapping: Type ${tpe} doesn't exist in schema:\n$s"))
    }

}

trait SchemaInstances {

  implicit val SemigroupDirective: Semigroup[Directive] = (a, b) =>
    if (a.name != b.name) a
    else Directive(
      a.name,
      a.description orElse b.description,
      (a.locations ++ b.locations).distinct,
      (a.args ++ b.args).distinctBy(_.name)
    )

  implicit val SemigroupNamedType: Semigroup[NamedType] = {
    case ((a: ObjectType), (b: ObjectType)) =>
      ObjectType(
        a.name,
        a.description orElse b.description,
        (a.fields ++ b.fields).distinctBy(_.name),
        (a.interfaces ++ b.interfaces).distinctBy(_.name),
      )
    case (a, _) => a
  }

  implicit val SemigroupSchema: Semigroup[Schema] = (a, b) =>
    new Schema {
      val pos: SourcePos = a.pos
      val types: List[NamedType] =
        a.types.map(a => b.types.find(b => a.name === b.name).foldLeft(a)(_ |+| _)) ++
        b.types.filterNot(b => a.types.exists(_.name === b.name))
      lazy val directives: List[Directive] =
        a.directives.map(a => b.directives.find(b => a.name === b.name).foldLeft(a)(_ |+| _)) ++
        b.directives.filterNot(b => a.directives.exists(_.name === b.name))
    }

}

object SchemaInstances extends SchemaInstances


