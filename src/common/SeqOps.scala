package scala.virtualization.lms
package common

import java.io.PrintWriter
import internal._
import scala.reflect.SourceContext

trait SeqOps extends Variables with BooleanOps {

  object Seq {
    def apply[A:Manifest](xs: Rep[A]*)(implicit pos: SourceContext) = seq_new(xs)
  }

  implicit def varToSeqOps[A:Manifest](x: Var[Seq[A]]) = new SeqOpsCls(readVar(x))
  implicit def repSeqToSeqOps[T:Manifest](a: Rep[Seq[T]]) = new SeqOpsCls(a)
  implicit def seqToSeqOps[T:Manifest](a: Seq[T]) = new SeqOpsCls(unit(a))

  class SeqOpsCls[T:Manifest](self: Rep[Seq[T]]){
    def apply(n: Rep[Int])(implicit pos: SourceContext) = seq_apply(self,n)
    def length(implicit pos: SourceContext) = seq_length(self)
    def map[U:Manifest](f: Rep[T] => Rep[U]) = seq_map(self,f)
    def foldLeft[U:Manifest](z: Rep[U])(f: (Rep[U],Rep[T]) => Rep[U]) = seq_foldl(self,z,f)
    def max(implicit cmp: Ordering[T]) = seq_max(self)
    def filter(p: Rep[T] => Rep[Boolean]) = seq_filter(self, p)
    def filterNot(p: Rep[T] => Rep[Boolean]) = seq_filter(self, ((y: Rep[T]) => !p(y)))
  }

  def seq_new[A:Manifest](xs: Seq[Rep[A]])(implicit pos: SourceContext): Rep[Seq[A]]
  def seq_apply[T:Manifest](x: Rep[Seq[T]], n: Rep[Int])(implicit pos: SourceContext): Rep[T]
  def seq_length[T:Manifest](x: Rep[Seq[T]])(implicit pos: SourceContext): Rep[Int]
  def seq_map[A:Manifest,B:Manifest](xs: Rep[Seq[A]], f: Rep[A] => Rep[B])(implicit pos: SourceContext): Rep[Seq[B]]
  def seq_foldl[A:Manifest,B:Manifest](xs: Rep[Seq[A]], z: Rep[B], f: (Rep[B],Rep[A]) => Rep[B])(implicit pos: SourceContext): Rep[B]

  def infix_flatten[A:Manifest](xs: Rep[Seq[Seq[A]]])(implicit pos: SourceContext) = seq_flatten(xs)

  def seq_flatten[A:Manifest](xs: Rep[Seq[Seq[A]]])(implicit pos: SourceContext): Rep[Seq[A]]
  def seq_max[A:Manifest:Ordering](xs: Rep[Seq[A]])(implicit pos: SourceContext): Rep[A]
  def seq_filter[A:Manifest](xs: Rep[Seq[A]], p: Rep[A] => Rep[Boolean])(implicit pos: SourceContext): Rep[Seq[A]]
}

trait SeqOpsExp extends SeqOps with EffectExp with BooleanOpsExp {
  case class SeqNew[A:Manifest](xs: List[Rep[A]]) extends Def[Seq[A]]
  case class SeqLength[T:Manifest](a: Exp[Seq[T]]) extends Def[Int]
  case class SeqApply[T:Manifest](x: Exp[Seq[T]], n: Exp[Int]) extends Def[T]
  case class SeqMap[A:Manifest,B:Manifest](xs: Exp[Seq[A]], x: Sym[A], block: Block[B]) extends Def[Seq[B]]
  case class SeqFoldl[A:Manifest,B:Manifest](xs: Exp[Seq[A]], zero: Block[B], acc: Sym[B], x: Sym[A], block: Block[B]) extends Def[B]
  case class SeqFlatten[A:Manifest](xs: Exp[Seq[Seq[A]]]) extends Def[Seq[A]]
  case class SeqMax[A:Manifest](xs: Exp[Seq[A]], cmp: Ordering[A]) extends Def[A]
  case class SeqFilter[A:Manifest](xs: Exp[Seq[A]], x: Sym[A], block: Block[Boolean]) extends Def[Seq[A]]

  def seq_new[A:Manifest](xs: Seq[Rep[A]])(implicit pos: SourceContext) = SeqNew(xs.toList)
  def seq_apply[T:Manifest](x: Exp[Seq[T]], n: Exp[Int])(implicit pos: SourceContext): Exp[T] = SeqApply(x, n)
  def seq_length[T:Manifest](a: Exp[Seq[T]])(implicit pos: SourceContext): Exp[Int] = SeqLength(a)
  def seq_map[A:Manifest,B:Manifest](xs: Exp[Seq[A]], f: Exp[A] => Exp[B])(implicit pos: SourceContext) = {
    val a = fresh[A]
    val b = reifyEffects(f(a))
    reflectEffect(SeqMap(xs, a, b), summarizeEffects(b).star)
  }
  def seq_foldl[A:Manifest,B:Manifest](xs: Exp[Seq[A]], z: Exp[B], f: (Exp[B],Exp[A]) => Exp[B])(implicit pos: SourceContext) = {
    val acc = fresh[B]
    val elem = fresh[A]
    val b = reifyEffects(f(acc,elem))
    reflectEffect(SeqFoldl(xs,reifyEffects(z),acc,elem,b), summarizeEffects(b).star)
  }

  def seq_flatten[A:Manifest](xs: Exp[Seq[Seq[A]]])(implicit pos: SourceContext): Exp[Seq[A]] = SeqFlatten(xs)

  def seq_max[A:Manifest:Ordering](xs: Exp[Seq[A]])(implicit pos: SourceContext) =
    SeqMax(xs, implicitly[Ordering[A]])

  def seq_filter[A:Manifest](xs: Exp[Seq[A]], p: Exp[A] => Exp[Boolean])(implicit pos: SourceContext) = {
    val a = fresh[A]
    val b = reifyEffects(p(a))
    reflectEffect(SeqFilter(xs,a,b), summarizeEffects(b).star)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case SeqNew(xs) => seq_new(f(xs))
    case SeqFlatten(xs) => seq_flatten(f(xs))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  override def syms(e: Any): List[Sym[Any]] = e match {
    case SeqNew(xs) => (xs flatMap { syms }).toList
    case SeqMap(xs, x, body) => syms(xs):::syms(body)
    case SeqFoldl(xs, z, acc, x, body) => syms(xs):::syms(body)
    case SeqFilter(xs, _, body) => syms(xs) ::: syms(body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case SeqMap(xs, x, body) => x :: effectSyms(body)
    case SeqFoldl(xs, z, acc, x, body) => acc :: x :: effectSyms(body)
    case SeqFilter(_, x, body) => x :: effectSyms(body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case SeqNew(xs) => (xs flatMap { freqNormal }).toList
    case SeqMap(xs, x, body) => freqNormal(xs):::freqHot(body)
    case SeqFoldl(xs, z, acc, x, body) => freqNormal(xs):::freqHot(body)
    case SeqFilter(xs, _, body) => freqNormal(xs) ::: freqHot(body)
    case _ => super.symsFreq(e)
  }
}

trait BaseGenSeqOps extends GenericNestedCodegen {
  val IR: SeqOpsExp
  import IR._

}

trait ScalaGenSeqOps extends BaseGenSeqOps with ScalaGenEffect with ScalaGenBooleanOps {
  val IR: SeqOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case SeqNew(xs) => emitValDef(sym, src"Seq($xs)")
    case SeqLength(x) => emitValDef(sym, src"$x.length")
    case SeqApply(x,n) => emitValDef(sym, src"$x($n)")
    case SeqFlatten(xs) => emitValDef(sym, src"$xs.flatten")
    case SeqMax(xs,cmp) => emitValDef(sym, src"$xs.max")
    case SeqMap(xs,x,blk) =>
      gen"""val $sym = $xs.map { $x =>
           |${nestedBlock(blk)}
           |$blk
           |}"""
    case SeqFoldl(xs,z,acc,x,blk) =>
      gen"""val $sym = $xs.foldLeft($z) { ($acc,$x) =>
           |${nestedBlock(blk)}
           |$blk
           } """
      // emitValDef(sym, quote(xs) + ".foldLeft{")
      // emitBlock(z)
      // stream.println("} { " + quote(x) + " =>")
      // emitBlock(blk)
      // stream.println("}")
    case SeqFilter(xs, x, b) =>
      gen"""val $sym = $xs.filter { $x =>
           |${nestedBlock(b)}
           |$b
           |}"""

    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenSeqOps extends BaseGenSeqOps with CLikeGenBase  {
  val IR: SeqOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case _ => super.emitNode(sym, rhs)
    }
  }
}

trait CudaGenSeqOps extends CudaGenEffect with CLikeGenSeqOps
trait OpenCLGenSeqOps extends OpenCLGenEffect with CLikeGenSeqOps
trait CGenSeqOps extends CGenEffect with CLikeGenSeqOps
