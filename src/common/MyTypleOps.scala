package scala.virtualization.lms
package common

import scala.virtualization.lms.internal.GenericCodegen
import scala.virtualization.lms.internal.Expressions
import java.io.PrintWriter
import scala.reflect.SourceContext

trait MyTupleOps extends Base {

  object P2 {
    def apply[A:Manifest,B:Manifest](fst: Rep[A], snd: Rep[B]) = my_make_tuple2((fst,snd))
  }

  object P3 {
    def apply[A:Manifest,B:Manifest,C:Manifest](fst: Rep[A], snd: Rep[B], trd: Rep[C]) = my_make_tuple3((fst,snd,trd))
  }

  object P4 {
    def apply[A:Manifest,B:Manifest,C:Manifest,D:Manifest](
      fst: Rep[A], snd: Rep[B], trd: Rep[C], fth: Rep[D]) = my_make_tuple4((fst,snd,trd,fth))
  }

  object P5 {
    def apply[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest](
      fst: Rep[A], snd: Rep[B], trd: Rep[C], fth: Rep[D], fif: Rep[E]) =
      my_make_tuple5((fst,snd,trd,fth,fif))
  }

  implicit def my_make_tuple2[A:Manifest,B:Manifest](t: (Rep[A], Rep[B]))(implicit pos: SourceContext) : Rep[(A,B)]
  implicit def my_make_tuple3[A:Manifest,B:Manifest,C:Manifest](t: (Rep[A], Rep[B], Rep[C]))(implicit pos: SourceContext) : Rep[(A,B,C)]
  implicit def my_make_tuple4[A:Manifest,B:Manifest,C:Manifest,D:Manifest](t: (Rep[A], Rep[B], Rep[C], Rep[D]))(implicit pos: SourceContext) : Rep[(A,B,C,D)]
  implicit def my_make_tuple5[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest](t: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E]))(implicit pos: SourceContext) : Rep[(A,B,C,D,E)]

  def my_tuple2_get1[A:Manifest](t: Rep[(A,_)])(implicit pos: SourceContext) : Rep[A]
  def my_tuple2_get2[B:Manifest](t: Rep[(_,B)])(implicit pos: SourceContext) : Rep[B]

  def my_tuple3_get1[A:Manifest](t: Rep[(A,_,_)])(implicit pos: SourceContext) : Rep[A]
  def my_tuple3_get2[B:Manifest](t: Rep[(_,B,_)])(implicit pos: SourceContext) : Rep[B]
  def my_tuple3_get3[C:Manifest](t: Rep[(_,_,C)])(implicit pos: SourceContext) : Rep[C]

  def my_tuple4_get1[A:Manifest](t: Rep[(A,_,_,_)])(implicit pos: SourceContext) : Rep[A]
  def my_tuple4_get2[B:Manifest](t: Rep[(_,B,_,_)])(implicit pos: SourceContext) : Rep[B]
  def my_tuple4_get3[C:Manifest](t: Rep[(_,_,C,_)])(implicit pos: SourceContext) : Rep[C]
  def my_tuple4_get4[D:Manifest](t: Rep[(_,_,_,D)])(implicit pos: SourceContext) : Rep[D]

  def my_tuple5_get1[A:Manifest](t: Rep[(A,_,_,_,_)])(implicit pos: SourceContext) : Rep[A]
  def my_tuple5_get2[B:Manifest](t: Rep[(_,B,_,_,_)])(implicit pos: SourceContext) : Rep[B]
  def my_tuple5_get3[C:Manifest](t: Rep[(_,_,C,_,_)])(implicit pos: SourceContext) : Rep[C]
  def my_tuple5_get4[D:Manifest](t: Rep[(_,_,_,D,_)])(implicit pos: SourceContext) : Rep[D]
  def my_tuple5_get5[E:Manifest](t: Rep[(_,_,_,_,E)])(implicit pos: SourceContext) : Rep[E]
}

trait MyTupleOpsExp extends MyTupleOps with Expressions {

  case class CP2[A:Manifest,B:Manifest](fst: Exp[A], snd: Exp[B]) extends Def[(A,B)]
  case class CP3[A:Manifest,B:Manifest,C:Manifest](fst: Exp[A], snd: Exp[B], trd: Exp[C]) extends Def[(A,B,C)]
  case class CP4[A:Manifest,B:Manifest,C:Manifest,D:Manifest](fst: Exp[A], snd: Exp[B], trd: Exp[C], fth: Exp[D]) extends Def[(A,B,C,D)]
  case class CP5[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest](fst: Exp[A], snd: Exp[B], trd: Exp[C], fth: Exp[D], fif: Exp[E]) extends Def[(A,B,C,D,E)]

  case class P2Get1[A:Manifest](p: Exp[(A,_)]) extends Def[A]
  case class P2Get2[B:Manifest](p: Exp[(_,B)]) extends Def[B]

  case class P3Get1[A:Manifest](p: Exp[(A,_,_)]) extends Def[A]
  case class P3Get2[B:Manifest](p: Exp[(_,B,_)]) extends Def[B]
  case class P3Get3[C:Manifest](p: Exp[(_,_,C)]) extends Def[C]

  case class P4Get1[A:Manifest](p: Exp[(A,_,_,_)]) extends Def[A]
  case class P4Get2[B:Manifest](p: Exp[(_,B,_,_)]) extends Def[B]
  case class P4Get3[C:Manifest](p: Exp[(_,_,C,_)]) extends Def[C]
  case class P4Get4[D:Manifest](p: Exp[(_,_,_,D)]) extends Def[D]

  case class P5Get1[A:Manifest](p: Exp[(A,_,_,_,_)]) extends Def[A]
  case class P5Get2[B:Manifest](p: Exp[(_,B,_,_,_)]) extends Def[B]
  case class P5Get3[C:Manifest](p: Exp[(_,_,C,_,_)]) extends Def[C]
  case class P5Get4[D:Manifest](p: Exp[(_,_,_,D,_)]) extends Def[D]
  case class P5Get5[E:Manifest](p: Exp[(_,_,_,_,E)]) extends Def[E]

  implicit def my_make_tuple2[A:Manifest,B:Manifest](t: (Exp[A],Exp[B]))(implicit pos: SourceContext) : Exp[(A,B)] = CP2(t._1,t._2)
  implicit def my_make_tuple3[A:Manifest,B:Manifest,C:Manifest](t: (Exp[A],Exp[B],Exp[C]))(implicit pos: SourceContext) : Exp[(A,B,C)] = CP3(t._1,t._2,t._3)
  implicit def my_make_tuple4[A:Manifest,B:Manifest,C:Manifest,D:Manifest](t: (Exp[A],Exp[B],Exp[C],Exp[D]))(implicit pos: SourceContext) : Exp[(A,B,C,D)] = CP4(t._1,t._2,t._3,t._4)
  implicit def my_make_tuple5[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest](t: (Exp[A],Exp[B],Exp[C],Exp[D],Exp[E]))(implicit pos: SourceContext) : Exp[(A,B,C,D,E)] = CP5(t._1,t._2,t._3,t._4, t._5)

  def my_tuple2_get1[A:Manifest](p: Exp[(A,_)])(implicit pos: SourceContext) = P2Get1(p)
  def my_tuple2_get2[B:Manifest](p: Exp[(_,B)])(implicit pos: SourceContext) = P2Get2(p)

  def my_tuple3_get1[A:Manifest](p: Exp[(A,_,_)])(implicit pos: SourceContext) = P3Get1(p)
  def my_tuple3_get2[B:Manifest](p: Exp[(_,B,_)])(implicit pos: SourceContext) = P3Get2(p)
  def my_tuple3_get3[C:Manifest](p: Exp[(_,_,C)])(implicit pos: SourceContext) = P3Get3(p)

  def my_tuple4_get1[A:Manifest](p: Exp[(A,_,_,_)])(implicit pos: SourceContext) = P4Get1(p)
  def my_tuple4_get2[B:Manifest](p: Exp[(_,B,_,_)])(implicit pos: SourceContext) = P4Get2(p)
  def my_tuple4_get3[C:Manifest](p: Exp[(_,_,C,_)])(implicit pos: SourceContext) = P4Get3(p)
  def my_tuple4_get4[D:Manifest](p: Exp[(_,_,_,D)])(implicit pos: SourceContext) = P4Get4(p)

  def my_tuple5_get1[A:Manifest](p: Exp[(A,_,_,_,_)])(implicit pos: SourceContext) = P5Get1(p)
  def my_tuple5_get2[B:Manifest](p: Exp[(_,B,_,_,_)])(implicit pos: SourceContext) = P5Get2(p)
  def my_tuple5_get3[C:Manifest](p: Exp[(_,_,C,_,_)])(implicit pos: SourceContext) = P5Get3(p)
  def my_tuple5_get4[D:Manifest](p: Exp[(_,_,_,D,_)])(implicit pos: SourceContext) = P5Get4(p)
  def my_tuple5_get5[E:Manifest](p: Exp[(_,_,_,_,E)])(implicit pos: SourceContext) = P5Get5(p)
}

trait ScalaGenMyTupleOps extends GenericCodegen {
  val IR: MyTupleOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {

    case CP2(fst, snd) => emitValDef(sym, "(" + quote(fst) + "," + quote(snd) + ")")
    case CP3(fst, snd, trd) => emitValDef(sym, "(" + quote(fst) + "," + quote(snd) + "," + quote(trd) + ")")
    case CP4(fst, snd, trd, fth) => emitValDef(sym, "(" + quote(fst) + "," + quote(snd) + "," + quote(trd) + "," + quote(fth) + ")")
    case CP5(fst, snd, trd, fth, fif) => emitValDef(sym, "(" + quote(fst) + "," + quote(snd) + "," + quote(trd) + "," + quote(fth) + "," + quote(fif) + ")")

    case P2Get1(p) => emitValDef(sym, quote(p) + "._1")
    case P2Get2(p) => emitValDef(sym, quote(p) + "._2")

    case P3Get1(p) => emitValDef(sym, quote(p) + "._1")
    case P3Get2(p) => emitValDef(sym, quote(p) + "._2")
    case P3Get3(p) => emitValDef(sym, quote(p) + "._3")

    case P4Get1(p) => emitValDef(sym, quote(p) + "._1")
    case P4Get2(p) => emitValDef(sym, quote(p) + "._2")
    case P4Get3(p) => emitValDef(sym, quote(p) + "._3")
    case P4Get4(p) => emitValDef(sym, quote(p) + "._4")

    case P5Get1(p) => emitValDef(sym, quote(p) + "._1")
    case P5Get2(p) => emitValDef(sym, quote(p) + "._2")
    case P5Get3(p) => emitValDef(sym, quote(p) + "._3")
    case P5Get4(p) => emitValDef(sym, quote(p) + "._4")
    case P5Get5(p) => emitValDef(sym, quote(p) + "._5")

    case _ => super.emitNode(sym, rhs)
  }
}
