// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.util

import cats.syntax.all.*
import grackle.Directive
import grackle.Env
import grackle.Env.EmptyEnv
import grackle.Env.NonEmptyEnv
import grackle.PathTerm
import grackle.Predicate
import grackle.Query
import grackle.Query.*
import grackle.Term
import org.typelevel.paiges.Doc
import org.typelevel.paiges.Doc.char
import org.typelevel.paiges.Doc.intercalate
import org.typelevel.paiges.Doc.line
import org.typelevel.paiges.Doc.space
import org.typelevel.paiges.Doc.str
import org.typelevel.paiges.Doc.text

/** Paiges combinators for Grackle queries. */
object PrettyPrinter {

  object Paren:
    val Open = char('(')
    val Close = char(')')

  object Bracket:
    val Open = char('[')
    val Close = char(']')

  val Comma = char(',')

  def prop(key: String, value: Doc): Doc =
    (text(key) + char(':') + space + value)

  def elems(ds: List[Doc], delim: Doc = Comma): Doc =
    intercalate(Comma + line, ds).grouped

  def quoted(s: String): Doc =
    char('"') + text(s) + char('"')

  def obj(name: String, ps: (String, Doc)*): Doc =
    obj(name, ps.map(prop)*)

  def obj(name: String, vs: Doc*)(using DummyImplicit): Doc =
    elems(vs.toList).tightBracketBy(text(name) + Paren.Open, Paren.Close)

  def binding(b: Binding): Doc =
    obj("Binding", "name" -> quoted(b.name), "value" -> str(b.value))

  def directives(ds: List[Directive]): Doc =
    elems(ds.map { d => prop(d.name, elems(d.args.map(binding))) })

  def query(q: Query): Doc =
    q match
      case Component(mapping, join, child) => obj("Component", "mapping" -> str("<mapping>"), "join" -> str("<function>") , "child" -> query(child))
      case Count(child) => ???
      case Effect(handler, child)          => obj("Effect", "handler" -> str("<handler>"), "child" -> query(child))
      case Empty                           => text("Empty")
      case Environment(e, child)           => obj("Environment", "env" -> env(e), "child" -> query(child))
      case Filter(pred, child)             => obj("Filter", "pred" -> predicate(pred), "child" -> query(child))
      case Group(queries)                  => obj("Group", queries.map(query)*)
      case Introspect(schema, child)       => obj("Introspect", "schema" -> str("<schema>"), "child" -> query(child))
      case Limit(num, child)               => obj("Limit", "num" -> str(num), "child" -> query(child))
      case Narrow(subtpe, child)           => obj("Narrow", "subtpe" -> str(subtpe), "child" -> query(child))
      case Offset(num, child)              => obj("Offset", "num" -> str(num), "child" -> query(child))
      case OrderBy(selections, child)      => obj("OrderBy", "selections" -> orderSelections(selections), "child" -> query(child))
      case Select(name, alias, child) =>
        var props = List("name" -> quoted(name)) ++ alias.foldMap(a => List("alias" -> quoted(a)))
        if child != Query.Empty then props :+= "child" -> query(child)
        if props.length == 1 then obj("Select", props.head._2) else obj("Select", props*)
      case TransformCursor(f, child)       => obj("TransformCursor", "f" -> text("<function>"), "child" -> query(child))
      case Unique(child)                   => obj("Unique", query(child))
      case UntypedFragmentSpread(name, ds) => obj("UntypedFragmentSpread", "directives" -> directives(ds))
      case UntypedInlineFragment(tpnme, ds, child) => 
        val ps = tpnme.foldMap(n => List("tpnme" -> quoted(n))) :+ ("directives" -> directives(ds)) :+ ("child" -> query(child))
        obj("UntypedInlineFragment", ps*)
      case UntypedSelect(name, alias, args, ds, child) =>
        var props = List("name" -> quoted(name)) ++ alias.foldMap(a => List("alias" -> quoted(a)))
        if args.nonEmpty then props = props :+ ("args" -> elems(args.map(binding)).tightBracketBy(Bracket.Open, Bracket.Close))
        if child != Query.Empty then props :+= "child" -> query(child)
        if props.length == 1 then obj("Select", props.head._2) else obj("Select", props*)

  def env(e: Env): Doc =
    e match
      case EmptyEnv => text("{}")
      case NonEmptyEnv(kvs) => elems(kvs.toList.map((k, v) => prop(k, str(v)))).tightBracketBy(char('{'), char('}'))

  def orderSelections(sel: OrderSelections): Doc =
    elems(sel.selections.map(orderSelection)).tightBracketBy(Bracket.Open, Bracket.Close)

  def orderSelection[A](sel: OrderSelection[A]): Doc =
    obj("OrderSelection", "term" -> term(sel.term), "ascending" -> str(sel.ascending), "nullsLast" -> str(sel.nullsLast))

  def predicate(pred: Predicate): Doc =
    pred match
      case Predicate.And(x, y)             => obj("And", predicate(x), predicate(y))
      case Predicate.Contains(a, as)       => obj("Contains", term(a), term(as))
      case Predicate.Eql(x, y)             => obj("Eql", term(x), term(y))
      case Predicate.False                 => text("False")
      case Predicate.Gt(x, y)              => obj("Gt", term(x), term(y))
      case Predicate.GtEql(x, y)           => obj("GtEql", term(x), term(y))
      case Predicate.In(t, ts)             => obj("In", term(t), elems(ts.map(str)).tightBracketBy(Bracket.Open, Bracket.Close))
      case Predicate.IsNull(x, isNull)     => obj("IsNull", term(x), str(isNull))
      case Predicate.Lt(x, y)              => obj("Lt", term(x), term(y))
      case Predicate.LtEql(x, y)           => obj("LtEql", term(x), term(y))
      case Predicate.Matches(x, r)         => obj("Matches", term(x), str(r))
      case Predicate.NEql(x, y)            => obj("NEql", term(x), term(y))
      case Predicate.Not(x)                => obj("Not", predicate(x))
      case Predicate.Or(x, y)              => obj("Or", predicate(x), predicate(y))
      case Predicate.StartsWith(x, prefix) => obj("StartsWith", term(x), quoted(prefix))
      case Predicate.True                  => text("True")
      case p                               => text(s"<Predicate:${p.getClass.getSimpleName}>")

  def term[A](t: Term[A]): Doc =
    t match
      case p: Predicate => predicate(p) // Predicate <: Term[Boolean]
      case PathTerm.ListPath(ss) => obj("ListPath", ss.map(quoted)*)
      case PathTerm.UniquePath(ss) => obj("UniquePath", ss.map(quoted)*)
      case Predicate.Const(a: String) => obj("Const", quoted(a))
      case Predicate.Const(a) => obj("Const", str(a))
      case Predicate.ToLowerCase(x) => obj("ToLowerCase", term(x))
      case Predicate.ToUpperCase(x) => obj("ToUpperCase", term(x))
      case t => text(s"<Term:${t.getClass.getSimpleName}>")

}
