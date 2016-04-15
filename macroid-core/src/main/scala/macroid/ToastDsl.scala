package macroid

import android.widget.Toast
import android.view.View

// Loaf is an operation mutate Toast
case class Loaf(f: Toast ⇒ Unit) {
  def apply(t: Toast) = f(t)
}

private[macroid] trait Loafs {
  /** Make the toast long */
  val long = Loaf(_.setDuration(Toast.LENGTH_LONG))

  /** Change the gravity of the toast */
  def gravity(g: Int, xOffset: Int = 0, yOffset: Int = 0) = Loaf(_.setGravity(g, xOffset, yOffset))

  /** Show the toast */
  val fry = Loaf(_.show())
}

object Loafs extends Loafs

// utilities for Ui[Toast] creation
private[macroid] trait ToastBuilding {
  /** Create a toast with the specified text */
  def toast(text: CharSequence)(implicit ctx: ContextWrapper): Ui[Toast] =
    Ui(Toast.makeText(ctx.getOriginal, text, Toast.LENGTH_SHORT))

  /** Create a toast with the specified resource ID */
  def toast(text: Int)(implicit ctx: ContextWrapper): Ui[Toast] =
    Ui(Toast.makeText(ctx.getOriginal, text, Toast.LENGTH_SHORT))

  /** Create a toast with either the specified text or the specified resource ID */
  def toast(text: Either[Int, CharSequence])(implicit ctx: ContextWrapper): Ui[Toast] = text match {
    case Right(t) ⇒ Ui(Toast.makeText(ctx.getOriginal, t, Toast.LENGTH_SHORT))
    case Left(t) ⇒ Ui(Toast.makeText(ctx.getOriginal, t, Toast.LENGTH_SHORT))
  }

  /** Create a toast with the specified view */
  def toast(view: Ui[View])(implicit ctx: ContextWrapper): Ui[Toast] =
    //todo: find Toast's implicit conversion
    view.map(v ⇒ new Toast(ctx.getOriginal) { setView(v); setDuration(Toast.LENGTH_SHORT) })
}

object ToastBuilding extends ToastBuilding

private[macroid] trait Loafing {
  // concat Loaf operations on Ui[Toast]
  implicit class LoafingOps(toast: Ui[Toast]) {
    def <~(loaf: Loaf) = toast map { t ⇒ loaf(t); t }
  }
}

object Loafing extends Loafing
