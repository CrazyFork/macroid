package macroid.util

import macroid._

import scala.language.higherKinds
import scala.concurrent.{ Future, ExecutionContext }
import scala.util.Try


/*
this module easy container Types for calling UiAction on its items

container including:
  TraversableOnce
  Option
  Try
  Future
 */

trait Effector[-F[_]] {
  def foreach[A](fa: F[A])(f: A ⇒ Ui[Any]): Unit
}

object Effector {

  implicit object `TraversableOnce is Effector` extends Effector[TraversableOnce] {
    override def foreach[A](fa: TraversableOnce[A])(f: A ⇒ Ui[Any]): Unit = fa.foreach(a ⇒ f(a).run)
  }

  implicit object `Option is Effector` extends Effector[Option] {
    def foreach[A](fa: Option[A])(f: A ⇒ Ui[Any]) = fa.foreach(a ⇒ f(a).run)
  }

  implicit object `Try is Effector` extends Effector[Try] {
    def foreach[A](fa: Try[A])(f: A ⇒ Ui[Any]) = fa.foreach(a ⇒ f(a).run)
  }

  implicit def `Future is Effector`(implicit ec: ExecutionContext) = new Effector[Future] {
    // Future -> UiFuture 的implicit conversion 定义在了package.scala中
    def foreach[A](fa: Future[A])(f: A ⇒ Ui[Any]) = fa.mapUi(f)
  }
}
