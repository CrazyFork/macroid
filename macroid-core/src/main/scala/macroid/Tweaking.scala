package macroid

import scala.language.higherKinds
import android.view.{ ViewGroup, View }
import scala.annotation.implicitNotFound
import macroid.util.Effector

@implicitNotFound("Don't know how to tweak ${W} with ${T}. Try importing an instance of CanTweak[${W}, ${T}, ...].") /** A typeclass for 'tweakable' relation */
trait CanTweak[W, -T, R] {
  def tweak(w: W, t: T): Ui[R] //bm: w for widget, t for operation on w, R the result of operations on w
}

// defined what w and what operations can operate on w, and returns what as Ui[R]
object CanTweak {
  // bm: Widget, Layout 原样返回利于 chain call

  // operate Tweak on target Widget then return this Widget
  implicit def `Widget is tweakable with Tweak`[W <: View]: CanTweak[W, Tweak[W], W] =
    new CanTweak[W, Tweak[W], W] {
      def tweak(w: W, t: Tweak[W]) = t(w).withResult(w)
    }

  implicit def `Widget is tweakable with Snail`[W <: View]: CanTweak[W, Snail[W], W] =
    new CanTweak[W, Snail[W], W] {
      def tweak(w: W, s: Snail[W]) = s(w).withResult(w)
    }

  implicit def `Layout is tweakable with Transformer`[L <: ViewGroup]: CanTweak[L, Transformer, L] =
    new CanTweak[L, Transformer, L] {
      def tweak(l: L, t: Transformer) = t(l).withResult(l)
    }

  implicit def `Widget is tweakable with Effector`[W <: View, F[+_], T, R](implicit effector: Effector[F], canTweak: CanTweak[W, T, R]): CanTweak[W, F[T], W] =
    new CanTweak[W, F[T], W] {
      // 注意如果F是Future, 这里的 effector.foreach 也不需要传入implicit execution context,
      // 这里只是用 Ui 记录操作, 在返回的 Ui 上触发执行才需要
      def tweak(w: W, f: F[T]) = Ui { effector.foreach(f)(t ⇒ canTweak.tweak(w, t)); w }
    }
  /*
  todo: 这么复杂的type限定作者是怎么写出来的, F[+_] 的作用

    F[W] F: Supported Containers
    T: Tweak or Transform on target view
    W: View, Widget, any target
   */
  implicit def `Effector is tweakable`[W, F[+_], T, R](implicit effector: Effector[F], canTweak: CanTweak[W, T, R]): CanTweak[F[W], T, F[W]] =
    new CanTweak[F[W], T, F[W]] {
      def tweak(f: F[W], t: T) = Ui { effector.foreach(f)(w ⇒ canTweak.tweak(w, t)); f }
    }

  implicit def `Ui is tweakable`[W, T, R](implicit canTweak: CanTweak[W, T, R]): CanTweak[Ui[W], T, W] =
    new CanTweak[Ui[W], T, W] {
      def tweak(ui: Ui[W], t: T) = ui flatMap { w ⇒ canTweak.tweak(w, t).withResult(w) }
    }
}

/** This trait defines the tweaking operator (<~) */
private[macroid] trait Tweaking {
  // export tweak behaviour
  /** Tweaking operator */
  implicit class TweakingOps[W](w: W) {
    /** Apply a tweak */
    def <~[T, R](t: T)(implicit canTweak: CanTweak[W, T, R]): Ui[R] = canTweak.tweak(w, t)
  }
}
