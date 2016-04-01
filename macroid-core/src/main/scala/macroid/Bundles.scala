package macroid

import scala.language.experimental.macros
import android.os.Bundle
import macrocompat.bundle
import scala.reflect.macros.blackbox

//bm: private keywork is good for hiding implementations
private[macroid] trait Bundles {
  def bundle(pairs: (String, Any)*): Bundle = macro BundleMacros.bundleImpl

  //bm: import Bundles._ 的时候 这个implicit 就默认载入了
  implicit class BundleAddition(b: Bundle) {
    def +(other: Bundle) = {
      val c = new Bundle
      c.putAll(b)
      c.putAll(other)
      c
    }
  }
}

object Bundles extends Bundles
//todo:
@bundle
class BundleMacros(val c: blackbox.Context) {
  import c.universe._

  def bundleImpl(pairs: c.Expr[(String, Any)]*): Tree = {
    val (singular, plural) = weakTypeOf[Bundle].members
      .filter(_.name.toString.startsWith("put"))
      .filterNot(_.name.toString == "putAll")
      .map(_.asMethod)
      .partition(x ⇒ (!x.name.toString.endsWith("Array") && !x.name.toString.endsWith("List")) || x.name.toString.contains("Sparse"))
    val (plain, able) = singular.partition(!_.name.toString.endsWith("able"))
    val b = TermName(c.freshName("bundle"))
    val puts = pairs map { pair ⇒
      val TypeRef(_, _, List(_, value)) = pair.actualType
      val put = plain.find(value =:= _.paramLists(0)(1).typeSignature) orElse
        able.find(value <:< _.paramLists(0)(1).typeSignature) getOrElse
        c.abort(pair.tree.pos, s"Could not put $value in a Bundle")
      q"$b.$put($pair._1, $pair._2)"
    }
    q"val $b = new _root_.android.os.Bundle; ..$puts; $b"
  }
}
