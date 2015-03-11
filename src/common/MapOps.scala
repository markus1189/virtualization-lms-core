package scala.virtualization.lms
package common
import internal._
import scala.reflect.SourceContext

trait MapOps extends Variables {
  object Map {
    def apply[A:Manifest,B:Manifest](kvs: Rep[(A,B)]*)(implicit pos: SourceContext) =
      map_new(kvs)
  }

  implicit def varToMapOps[A:Manifest,B:Manifest](x: Var[Map[A,B]]) =
    new MapOpsCls(readVar(x))
  implicit def repMapToMapOps[A:Manifest,B:Manifest](m: Rep[Map[A,B]]) = new MapOpsCls(m)
  implicit def mapToMapOps[A:Manifest,B:Manifest](m: Map[A,B]) = new MapOpsCls(unit(m))

  class MapOpsCls[A:Manifest,B:Manifest](m: Rep[Map[A,B]]) {
    def apply(key: Rep[A])(implicit pos: SourceContext) = map_apply(m,key)
    def withDefaultValue(z: Rep[B])(implicit pos: SourceContext) = map_with_default(m,z)
    def mapValues[C:Manifest](f: Rep[B] => Rep[C]) = map_map_values(m,f)
    def insert(kv: Rep[(A,B)])(implicit pos: SourceContext) = map_insert(m,kv)
    def filter(p: Rep[(A,B)] => Rep[Boolean])(implicit pos: SourceContext) = map_filter(m, p)
    def isEmpty(implicit pos: SourceContext) = map_is_empty(m)
    def values(implicit pos: SourceContext) = map_values(m)
    def toSeq(implicit pos: SourceContext) = map_to_seq(m)
  }

  def map_new[A:Manifest,B:Manifest](kvs: Seq[Rep[(A,B)]])(implicit pos: SourceContext): Rep[Map[A,B]]
  def map_apply[A:Manifest,B:Manifest](m: Rep[Map[A,B]], key: Rep[A])(implicit pos: SourceContext): Rep[B]
  def map_with_default[A:Manifest,B:Manifest](m: Rep[Map[A,B]], z: Rep[B])(implicit pos: SourceContext): Rep[Map[A,B]]
  def map_map_values[A:Manifest,B:Manifest,C:Manifest](m: Rep[Map[A,B]], f: Rep[B] => Rep[C])(implicit pos: SourceContext): Rep[Map[A,C]]
  def map_insert[A:Manifest,B:Manifest](m: Rep[Map[A,B]], kv: Rep[(A,B)])(implicit pos: SourceContext): Rep[Map[A,B]]
  def map_filter[A:Manifest,B:Manifest](m: Rep[Map[A,B]], p: Rep[(A,B)] => Rep[Boolean])(implicit pos: SourceContext): Rep[Map[A,B]]
  def map_is_empty[A:Manifest,B:Manifest](m: Rep[Map[A,B]]): Rep[Boolean]
  def map_values[A:Manifest,B:Manifest](m: Rep[Map[A,B]]): Rep[Seq[B]]
  def map_to_seq[A:Manifest,B:Manifest](m: Rep[Map[A,B]]): Rep[Seq[(A,B)]]
}

trait MapOpsExp extends MapOps with EffectExp {
  case class MapNew[A:Manifest,B:Manifest](xs: Seq[Exp[(A,B)]]) extends Def[Map[A,B]] {
    val mA = manifest[A]
    val mB = manifest[B]
  }
  case class MapApply[A:Manifest,B:Manifest](m: Exp[Map[A,B]], key: Exp[A]) extends Def[B]
  case class MapWithDefault[A:Manifest,B:Manifest](m: Exp
    [Map[A,B]], z: Exp[B]) extends Def[Map[A,B]]
  case class MapMapValues[A:Manifest,B:Manifest,C:Manifest](m: Exp[Map[A,B]], x: Sym[B], block: Block[C]) extends Def[Map[A,C]]
  case class MapInsert[A:Manifest,B:Manifest](m: Exp[Map[A,B]], kv: Exp[(A,B)]) extends Def[Map[A,B]]
  case class MapFilter[A:Manifest,B:Manifest](m: Exp[Map[A,B]], x: Sym[(A,B)], block: Block[Boolean]) extends Def[Map[A,B]]
  case class MapIsEmpty[A:Manifest,B:Manifest](m: Exp[Map[A,B]]) extends Def[Boolean]
  case class MapValues[A:Manifest,B:Manifest](m: Exp[Map[A,B]]) extends Def[Seq[B]]
  case class MapToSeq[A:Manifest,B:Manifest](m: Exp[Map[A,B]]) extends Def[Seq[(A,B)]]

  def map_new[A:Manifest,B:Manifest](kvs: Seq[Exp[(A,B)]])(implicit pos: SourceContext) =
    MapNew(kvs)

  def map_apply[A:Manifest,B:Manifest](m: Exp[Map[A,B]], key: Exp[A])(implicit pos: SourceContext) =
    MapApply(m,key)

  def map_with_default[A:Manifest,B:Manifest](m: Exp[Map[A,B]], z: Exp[B])(implicit pos: SourceContext) =
    reflectMutable(MapWithDefault(m,z))

  def map_map_values[A:Manifest,B:Manifest,C:Manifest](m: Exp[Map[A,B]], f: Exp[B] => Exp[C])(implicit pos: SourceContext) = {
    val x = fresh[B]
    val r = reifyEffects(f(x))
    reflectEffect(MapMapValues(m,x,r), summarizeEffects(r).star)
  }

  def map_filter[A:Manifest,B:Manifest](m: Exp[Map[A,B]], p: Exp[(A,B)] => Exp[Boolean])(implicit pos: SourceContext) = {
    val x = fresh[(A,B)]
    val b = reifyEffects(p(x))
    reflectEffect(MapFilter(m, x, b), summarizeEffects(b).star)
  }

  def map_insert[A:Manifest,B:Manifest](m: Exp[Map[A,B]], kv: Exp[(A,B)])(implicit pos: SourceContext) = MapInsert(m,kv)

  def map_is_empty[A:Manifest,B:Manifest](m: Exp[Map[A,B]]) =
    MapIsEmpty(m)

  def map_values[A:Manifest,B:Manifest](m: Exp[Map[A,B]]) =
    MapValues(m)

  def map_to_seq[A:Manifest,B:Manifest](m: Exp[Map[A,B]]) =
    MapToSeq(m)

  override def syms(e: Any): List[Sym[Any]] = e match {
    case MapNew(xs) => (xs flatMap { syms }).toList
    case MapMapValues(xs, x, body) => syms(xs):::syms(body)
    case MapFilter(a, _, body) => syms(a) ::: syms(body)
    case MapWithDefault(m,z) => syms(z) ::: syms(m)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case MapMapValues(xs, x, body) => x :: effectSyms(body)
    case MapFilter(_, x, body) => x :: effectSyms(body)
    case MapWithDefault(m,z) =>  syms(z) ::: effectSyms(m)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case MapNew(xs) => (xs flatMap { freqNormal }).toList
    case MapMapValues(xs, x, body) => freqNormal(xs):::freqHot(body)
    case MapFilter(a, _, body) => freqNormal(a) ::: freqHot(body)
    case MapWithDefault(m,z) => freqNormal(m) ::: freqHot(z)
    case _ => super.symsFreq(e)
  }
}

trait BaseGenMapOps extends GenericNestedCodegen {
  val IR: MapOpsExp
  import IR._
}

trait ScalaGenMapOps extends BaseGenMapOps with ScalaGenEffect {
  val IR: MapOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case m@MapNew(xs) => emitValDef(sym,src"Map[${m.mA},${m.mB}]($xs)")
    case MapApply(m, key) => emitValDef(sym,src"$m($key)")
    case MapWithDefault(m, z) => emitValDef(sym,src"$m.withDefaultValue($z)")
    case MapInsert(m, kv) => emitValDef(sym,src"$m + $kv")
    case MapIsEmpty(m) => emitValDef(sym,src"$m.isEmpty")
    case MapValues(m) => emitValDef(sym,src"$m.values.toSeq")
    case MapToSeq(m) => emitValDef(sym,src"$m.toSeq")
    case MapMapValues(m,x,block) =>
      gen"""val $sym = $m.mapValues { $x =>
           |${nestedBlock(block)}
           |$block
           |}"""
    case MapFilter(m, x, block) =>
      gen"""val $sym = $m.filter { $x =>
           |${nestedBlock(block)}
           |$block
           |}"""
    case _ => super.emitNode(sym, rhs)
  }
}
