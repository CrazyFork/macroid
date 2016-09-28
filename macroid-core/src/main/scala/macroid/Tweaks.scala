package macroid

import scala.language.dynamics
import scala.language.experimental.macros
import android.text.Html
import android.view.{ ViewGroup, View }
import android.widget.{ LinearLayout, TextView }
import macrocompat.bundle
import scala.reflect.macros.blackbox

private[macroid] trait BasicTweaks {
  /** Set this view’s id */
  def id(id: Int) = Tweak[View](_.setId(id))

  /** Attach arbitrary value to a view by means of setTag */
  def hold[A](value: A) = Tweak[View](_.setTag(value))

  /** Assign the view to the provided `var` */
  def wire[W <: View](v: W): Tweak[W] = macro BasicTweakMacros.wireImpl[W]
  /** Assign the view to the provided slot */
  def wire[W <: View](v: Option[W]): Tweak[W] = macro BasicTweakMacros.wireOptionImpl[W]
}

private[macroid] trait VisibilityTweaks {
  /** Hide this view (uses View.GONE) */
  val hide = Tweak[View](_.setVisibility(View.GONE))
  /** Show this view (uses View.VISIBLE) */
  val show = Tweak[View](_.setVisibility(View.VISIBLE))
  /** Conditionally show/hide this view */
  def show(c: Boolean): Tweak[View] = if (c) show else hide
}

private[macroid] trait AbilityTweaks {
  /** Disable this view */
  val disable = Tweak[View](_.setEnabled(false))
  /** Enable this view */
  val enable = Tweak[View](_.setEnabled(true))
  /** Conditionally enable/disable this view */
  def enable(c: Boolean): Tweak[View] = if (c) enable else disable
}

private[macroid] trait PaddingTweaks {
  // TODO: replace with setPaddingRelative!

  /** Set padding */
  def padding(left: Int = 0, top: Int = 0, right: Int = 0, bottom: Int = 0, all: Int = -1) = if (all >= 0) {
    Tweak[View](_.setPadding(all, all, all, all)) // _ stands for the targeted view component
  } else {
    Tweak[View](_.setPadding(left, top, right, bottom))
  }
}

private[macroid] trait LayoutTweaks {

  /** Use `LayoutParams` of the specified layout class */
  def layoutParams[L <: ViewGroup](params: Any*): Tweak[View] = macro LayoutTweakMacros.layoutParamsImpl[L]
  /** Use `LayoutParams` of the specified layout class */
  def lp[L <: ViewGroup](params: Any*): Tweak[View] = macro LayoutTweakMacros.layoutParamsImpl[L]

  /** Make this layout vertical */
  val vertical = Tweak[LinearLayout](_.setOrientation(LinearLayout.VERTICAL))
  /** Make this layout horizontal */
  val horizontal = Tweak[LinearLayout](_.setOrientation(LinearLayout.HORIZONTAL))

  /** Add views to the layout */
  def addViews(children: Seq[Ui[View]], removeOld: Boolean = false, reverse: Boolean = false) = Tweak[ViewGroup] { x ⇒
    if (removeOld) x.removeAllViews()
    children.foreach(c ⇒ if (reverse) x.addView(c.get, 0) else x.addView(c.get))
  }
}

private[macroid] trait TextTweaks {
  /** Set text */
  def text(text: CharSequence) = Tweak[TextView](_.setText(text))
  /** Set text */
  def text(text: Int) = Tweak[TextView](_.setText(text))
  /** Set text */
  def text(text: Either[Int, CharSequence]) = text match {
    case Right(t) ⇒ Tweak[TextView](_.setText(t))
    case Left(t) ⇒ Tweak[TextView](_.setText(t))
  }

  /** Set hint */
  def hint(hint: CharSequence) = Tweak[TextView](_.setHint(hint)) // Sets the text to be displayed when the text of the TextView is empty
  /** Set hint */
  def hint(hint: Int) = Tweak[TextView](_.setHint(hint)) // Sets the text to be displayed when the text of the TextView is empty, from a resource.
  /** Set hint */
  def hint(hint: Either[Int, CharSequence]) = hint match {
    case Right(t) ⇒ Tweak[TextView](_.setHint(t))
    case Left(t) ⇒ Tweak[TextView](_.setHint(t))
  }

  def html(html: String) = Tweak[TextView](_.setText(Html.fromHtml(html)))
}

private[macroid] trait EventTweaks {

  object On extends Dynamic {
    /** Set event handler */
    def applyDynamic[W <: View](event: String)(handler: Ui[Any]): Tweak[W] = macro EventTweakMacros.onUnitImpl[W]
  }

  object FuncOn extends Dynamic {
    /** Set event handler */
    def applyDynamic[W <: View](event: String)(handler: Any): Tweak[W] = macro EventTweakMacros.onFuncImpl[W]
  }
}

/** This trait provides the most useful tweaks. For an expanded set, see `contrib.ExtraTweaks` */

// bm: this is a technique make compose a larger module from smaller modules
private[macroid] trait Tweaks
  extends BasicTweaks
  with VisibilityTweaks
  with AbilityTweaks
  with PaddingTweaks
  with LayoutTweaks
  with TextTweaks
  with EventTweaks

object Tweaks extends Tweaks

/*
-todo-: ?
  http://docs.scala-lang.org/overviews/macros/bundles

  https://github.com/milessabin/macro-compat:
    macro-compat is a small library which, in conjunction with the macro-paradise
    compiler plugin, allows you to compile macros with Scala 2.10.x which are written
    to the Scala 2.11/2 macro API.

ok: this is test
 */
@bundle
class BasicTweakMacros(val c: blackbox.Context) {
  import c.universe._

  //todo: 这步会将最终的 view 绑定到 v 上? 还有为什么不直接 new 一个 Tweak 出来, 非要用 macro
  def wireImpl[W <: View: c.WeakTypeTag](v: c.Expr[W]): Tree = {
    q"_root_.macroid.Tweak[${weakTypeOf[W]}] { x ⇒ $v = x }"
  }
  // var content = slot[FrameLayout]
  // l[FrameLayout]() <~ wire(content) // assign L[FrageLayout] to content
  def wireOptionImpl[W <: View: c.WeakTypeTag](v: c.Expr[Option[W]]): Tree = {
    q"_root_.macroid.Tweak[${weakTypeOf[W]}] { x ⇒ $v = Some(x) }"
  }
}

@bundle
class LayoutTweakMacros(val c: blackbox.Context) {
  import c.universe._

  def layoutParams(l: c.Type, params: Seq[c.Expr[Any]]) = {
    // ${l.typeSymbol.companion} return companion object(subtype of Symbol of course) if existed
    q"_root_.macroid.Tweak[_root_.android.view.View] { x ⇒ x.setLayoutParams(new ${l.typeSymbol.companion}.LayoutParams(..$params)) }"
  }

  def findLayoutParams(layoutType: c.Type, params: Seq[c.Expr[Any]]): Tree = {
    var tp = layoutType

    // go up the inheritance chain until we find a suitable LayoutParams class in the companion
    //bm: c.typecheck is used to check whether this AST mutation
    // (dynamically Tweak creation operated on LayoutParams on unknown android view or scala Type)
    // could actually work
    while (scala.util.Try(c.typecheck(layoutParams(tp, params))).isFailure) {
      if (tp.baseClasses.length > 2) {
        tp = tp.baseClasses(1).asType.toType
      } else {
        c.abort(c.enclosingPosition, "Could not find the appropriate LayoutParams constructor")
      }
    }
    layoutParams(tp, params)
  }

  def layoutParamsImpl[L <: ViewGroup: c.WeakTypeTag](params: c.Expr[Any]*): Tree = {
    findLayoutParams(c.weakTypeOf[L], params)
  }
}

@bundle
class EventTweakMacros(val c: blackbox.Context) {
  import c.universe._

  // get all the info of binding event on target type
  private def onBase(event: c.Expr[String], tp: c.Type) = {
    // find the setter
    val Expr(Literal(Constant(eventName: String))) = event // use scala extractor to extract eventName out (as scala variable)
    val setter = scala.util.Try { // get setter method eg. setOnClickListener
      val s = tp.member(TermName(s"setOn${eventName.capitalize}Listener")).asMethod
      assert(s != NoSymbol); s
    } getOrElse {
      c.abort(c.enclosingPosition, s"Could not find method setOn${eventName.capitalize}Listener in $tp. You may need to provide the type argument explicitly")
    }

    // find the method to override
    val listener = setter.paramLists(0)(0).typeSignature // get first param, in scala params are pass as List[List[T]]
    val on = scala.util.Try {
      val x = listener.member(TermName(s"on${eventName.capitalize}")).asMethod
      assert(x != NoSymbol); x
    } getOrElse {
      c.abort(c.enclosingPosition, s"Unsupported event listener class in $setter")
    }

    /*
    button.setOnClickListener(new View.OnClickListener() {
         public void onClick(View v) {
             // Perform action on click
         }
     });

     in this case:
      setter:  button.setOnClickListener
      listener: View.OnClickListener
      on: public void onClick(View v)
      tp: Button
     */
    (setter, listener, on, tp) // setter, listener setter
  }

  sealed trait ListenerType
  object FuncListener extends ListenerType // 最终调用setOnclickListener 这样的 tweak
  object UnitListener extends ListenerType // call Ui[A].get on specific event triggering

  def getListener(tpe: c.Type, setter: c.universe.MethodSymbol, listener: c.Type, on: c.universe.MethodSymbol, f: c.Expr[Any], mode: ListenerType): Tree = {
    /*
    -todo-:
      freshName: generate unique name
        http://docs.scala-lang.org/overviews/quasiquotes/hygiene

      TermName 感觉就是一个 String wrapper
     */
    val args = on.paramLists(0).map(_ ⇒ TermName(c.freshName("arg")))
    val params = args zip on.paramLists(0) map { case (a, p) ⇒ q"val $a: ${p.typeSignature}" }
    lazy val argIdents = args.map(a ⇒ Ident(a))
    val impl = mode match {
      case FuncListener ⇒ q"$f(..$argIdents).get"
      case UnitListener ⇒ q"$f.get"
    }
    q"_root_.macroid.Tweak[$tpe] { x ⇒ x.$setter(new $listener { override def ${on.name.toTermName}(..$params) = $impl })}"
  }

  def onUnitImpl[W <: View: c.WeakTypeTag](event: c.Expr[String])(handler: c.Expr[Ui[Any]]): Tree = {
    val (setter, listener, on, tp) = onBase(event, weakTypeOf[W])
    scala.util.Try {
      /*
      scala.reflect.api.Exprs.Expr
        Type of the wrapped expression tree as found in the underlying tree.
        最终的 expression 的 type, eg.
          {println(3); 4} // 最终返回的应该是Int

      TypeRef:
        http://www.scala-lang.org/api/2.11.7/scala-reflect/index.html#scala.reflect.api.Types$TypeRef

      note `<:<` usage

       */
      if (!(on.returnType =:= typeOf[Unit]))
        // check whether Ui[Any]'s return type match(subtype of) target's view's event handler's return type defination
        assert((handler.actualType match { case TypeRef(_, _, t :: _) ⇒ t }) <:< on.returnType)
      getListener(tp, setter, listener, on, c.Expr(c.untypecheck(handler.tree)), UnitListener)
    } getOrElse {
      c.abort(c.enclosingPosition, s"handler should be of type Ui[${on.returnType}]")
    }
  }

  def onFuncImpl[W <: View: c.WeakTypeTag](event: c.Expr[String])(handler: c.Expr[Any]): Tree = {
    val (setter, listener, on, tp) = onBase(event, weakTypeOf[W])
    scala.util.Try {
      c.typecheck(getListener(tp, setter, listener, on, c.Expr(c.untypecheck(handler.tree)), FuncListener))
    } getOrElse {
      c.abort(c.enclosingPosition, s"handler should have type signature ${on.paramLists.head.mkString("(", ",", ")")}⇒Ui[${on.returnType}]")
    }
  }
}
