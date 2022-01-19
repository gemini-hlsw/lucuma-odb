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
import edu.gemini.grackle.Result
import cats.kernel.Monoid

trait SnippetMapping[F[_]] extends Mapping[F] with SchemaInstances with SelectElaboratorInstances {

  case class Snippet(
    schema: Schema,
    typeMappings: List[ObjectMapping],
    selectElaborator: QueryCompiler.SelectElaborator = Monoid.empty
  )

  implicit val SemigroupObjectMapping: Semigroup[ObjectMapping] = (a, b) =>
    if (!(a.tpe.asNamed.map(_.name) == b.tpe.asNamed.map(_.name))) a
    else ObjectMapping(
      a.tpe,
      (a.fieldMappings ++ b.fieldMappings).distinctBy(_.fieldName)
    )

  // Rereference a type in another schema. Failing to do this is an unrecoverable error.
  def reref(s: Schema): ObjectMapping => ObjectMapping = {
    case om @ ObjectMapping.DefaultObjectMapping(tpe, fm) =>
      s.ref(tpe).map(ObjectMapping(_, fm)(om.pos)).getOrElse(sys.error(s"SnippetMapping: Type ${om.tpe} doesn't exist in schema:\n$s"))
    case om => sys.error(s"SnippetMapping: I don't know how to retarget a type for a ${om.getClass.getName}")
  }

  implicit val SemigroupSnippetData: Semigroup[Snippet] = (a, b) => {
    val s1 = a.schema |+| b.schema
    Snippet(
      s1,
      (a.typeMappings.map(a => b.typeMappings.find(a.tpe.asNamed.map(_.name) === _.tpe.asNamed.map(_.name)).foldLeft(a)(_ |+| _)) ++
       b.typeMappings.filterNot(b => a.typeMappings.exists(_.tpe.asNamed.map(_.name) === b.tpe.asNamed.map(_.name)))).map(reref(s1)),
      rerefSE(s1)(a.selectElaborator) |+| rerefSE(s1)(b.selectElaborator)
    )
  }

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

  implicit val MonoidSelectElaborator: Monoid[QueryCompiler.SelectElaborator] =
    new Monoid[QueryCompiler.SelectElaborator] {
      def combine(a: QueryCompiler.SelectElaborator, b: QueryCompiler.SelectElaborator): QueryCompiler.SelectElaborator =
        new QueryCompiler.SelectElaborator(Map.empty) {
          override def transform(query: Query, vars: Query.Vars, schema: Schema, tpe: Type): Result[Query] =
            a.transform(query, vars, schema, tpe) orElse b.transform(query, vars, schema, tpe)
          override def elaborateArgs(tpe: Type, fieldName: String, args: List[Query.Binding]): Result[List[Query.Binding]] =
            a.elaborateArgs(tpe, fieldName, args) orElse b.elaborateArgs(tpe, fieldName, args)
        }
      val empty: QueryCompiler.SelectElaborator =
        new QueryCompiler.SelectElaborator(Map.empty)
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


