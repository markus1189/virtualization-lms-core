package scala.lms
package targets.clike.codegen

import ops.MiscOpsExp

import java.io.PrintWriter

trait CGenMiscOps extends CGenEffect {
  val IR: MiscOpsExp
  import IR._

  private def format(s: Exp[Any]): String = {
    remap(s.tp) match {
      case "CHAR" => "\"%c"
      case "bool" | "char" | "short" | "int" => "\"%d"
      case "long" => "\"%ld"
      case "float" | "double" => "\"%f"
      case _ => throw new Exception("CGenMiscOps: Non-primitive type error")
    }
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case PrintF(f,x) => stream.println("printf(" + ((Const(f:String)::x).map(quote)).mkString(",") + ");")
    case PrintLn(s) => stream.println("printf(\"%s\\n\"," + quote(s) + ");")
    case Print(s) => stream.println("printf(\"%s\"," + quote(s) + ");")
    case Exit(a) => stream.println("exit(" + quote(a) + ");")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenMiscOps extends CudaGenEffect {
  val IR: MiscOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}


trait OpenCLGenMiscOps extends OpenCLGenEffect {
  val IR: MiscOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}
