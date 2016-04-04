package macroid

import android.os.{ Handler, Looper }

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success, Try }

/** An ExecutionContext associated with the UI thread */
object UiThreadExecutionContext extends ExecutionContext {
  private lazy val uiHandler = new Handler(Looper.getMainLooper)

  def reportFailure(t: Throwable) = t.printStackTrace()
  def execute(runnable: Runnable) = uiHandler.post(runnable)
}

/** A UI action that can be sent to the UI thread for execution */
class Ui[+A](private[Ui] val v: () ⇒ A) {
  /** map combinator */
  //bm: is this lazy?check `functional programming in scala`
  //it is lazy, Ui will call apply, in method apply will wrap things in a new function
  def map[B](f: A ⇒ B) = Ui(f(v()))

  /** flatMap combinator */
  def flatMap[B](f: A ⇒ Ui[B]) = Ui(f(v()).v())

  /** Replace the resulting value with a new one */
  def withResult[B](result: B) = Ui { v(); result }

  /** Wait until this action is finished and replace the resulting value with a new one */
  def withResultAsync[B, C](result: B)(implicit evidence: A <:< Future[C], ec: ExecutionContext) = Ui {
    /*
    -todo-: `evidence(v())` type conversion?
    in Predef.scala
      sealed abstract class <:<[-From, +To] extends (From => To) with Serializable

    so evidence should be a Function[From, TO]

    so evidence should be pass by user, 
    todo: find this usage
     */
    evidence(v()) map (_ ⇒ result)
  }

  /** Combine (sequence) with another UI action */
  def ~[B](next: ⇒ Ui[B]) = Ui { v(); next.v() } //bm: {} vs ()
  // this could be implemented by flatMap
  // def ~[B](next: => Ui[B]):Ui[B] = flatMap( _ => next )

  /** Wait until this action is finished and combine (sequence) it with another one */
  def ~~[B, C](next: ⇒ Ui[B])(implicit evidence: A <:< Future[C]) = Ui {
    evidence(v()) mapUi (_ ⇒ next) // 这个 v() 换成 run 是不是更好
  }

  /** Run the action on the UI thread */
  def run: Future[A] = if (Ui.uiThread == Thread.currentThread) {
    Try(v()) match { // if v() throws Exception, Future.failed is called, otherwise Success with Result
      case Success(x) ⇒ Future.successful(x)
      case Failure(x) ⇒ Future.failed(x)
    }
  } else {
    /*
    -todo-: 为什么这一行就能确保是ui Thread, 而且这行的语义和上边已经明显不同了

      private[macroid] lazy val uiThread = Looper.getMainLooper.getThread

      语义还是相同的
     */
    Future(v())(UiThreadExecutionContext)
  }

  /** Get the result of executing the action on the current (hopefully, UI!) thread */
  def get = v()
}

object Ui {
  private[macroid] lazy val uiThread = Looper.getMainLooper.getThread

  /** A UI action that does nothing */
  def nop = Ui(())

  /** Create a UI action */
  def apply[A](v: ⇒ A) = new Ui(() ⇒ v)

  /** Combine (sequence) several UI actions together */
  // be careful, since `vs.map(_.v())` is still wrap in class Ui, so it is still lazy
  def sequence[A](vs: Ui[A]*) = Ui(vs.map(_.v()))

  /** Run a UI action on the UI thread */
  def run[A](ui: Ui[A]) = ui.run

  /** Run several UI actions on the UI thread */
  //trigger ui evaluation, v is defined in class UI defination
  def run[A](ui1: Ui[A], ui2: Ui[A], uis: Ui[A]*) = sequence(ui1 +: ui2 +: uis: _*).run

  /** Get the result of executing an UI action on the current (hopefully, UI!) thread */
  def get[A](ui: Ui[A]) = ui.get
}

/** Helpers to run UI actions as Future callbacks */
// more info about Future: http://docs.scala-lang.org/overviews/core/futures.html
case class UiFuture[T](future: Future[T]) extends AnyVal { //todo: why extends AnyVal
  /*
  -todo-: difference between Function and PartialFunction
          PartialFunction can be applied to Pattern Match in scala?
    
    PartialFunction means it only handle a subset of input params,
    and it can be used with case expression

    val paritalInt: List[Int] => Int = {
      case a :: b :: _ => b
    }
  
  See `Programming in scala page330` for more info    
   */
  private def applyUi[A, B](f: Function[A, Ui[B]]): Function[A, B] = x ⇒ f(x).get
  private def partialApplyUi[A, B](f: PartialFunction[A, Ui[B]]) = f andThen (_.get)

  /** Same as map, but performed on the UI thread.
    *
    * If the future is already completed and the current thread is the UI thread,
    * the UI action will be applied in-place, rather than asynchronously.
    *
    * mapUi(f: T => Ui[S]): Future[S]
    */
  def mapUi[S](f: Function[T, Ui[S]]) =
    if (future.isCompleted && Ui.uiThread == Thread.currentThread) {
      future.value.get.map(applyUi(f)) match {
        case Success(x) ⇒ Future.successful(x)
        case Failure(t) ⇒ Future.failed(t)
      }
    } else {
      future.map(applyUi(f))(UiThreadExecutionContext)
    }

  /** Same as flatMap, but performed on the UI thread
    *
    * If the future is already completed and the current thread is the UI thread,
    * the UI action will be applied in-place, rather than asynchronously.
    */
  def flatMapUi[S](f: Function[T, Ui[Future[S]]]) = {//return Future[X] not S
    if (future.isCompleted && Ui.uiThread == Thread.currentThread) {
      /*
      -todo-: Future[S] 在这步没有完成状态应该怎么算？
        上面已经判断了
       */
      future.value.get.map(applyUi(f)) match {
        case Success(x) ⇒ x
        case Failure(t) ⇒ Future.failed(t)
      }
    } else {
      future.flatMap(applyUi(f))(UiThreadExecutionContext)
    }
  }

  /** Same as foreach, but performed on the UI thread
    *
    * If the future is already completed and the current thread is the UI thread,
    * the UI action will be applied in-place, rather than asynchronously.
    */
  def foreachUi[U](f: Function[T, Ui[U]]):Unit =
    if (future.isCompleted && Ui.uiThread == Thread.currentThread) {
      future.value.get.foreach(applyUi(f))
    } else {
      future.foreach(applyUi(f))(UiThreadExecutionContext)
    }

  /** Same as recover, but performed on the UI thread */
  def recoverUi[U >: T](pf: PartialFunction[Throwable, Ui[U]]) =
    //recover[U >: T](pf: PartialFunction[Throwable, U])
    future.recover(partialApplyUi(pf))(UiThreadExecutionContext)

  /** Same as onSuccess, but performed on the UI thread */
  def onSuccessUi[U >: T](pf: PartialFunction[T, Ui[U]]) =
    future.onSuccess(partialApplyUi(pf))(UiThreadExecutionContext)

  /** Same as onFailure, but performed on the UI thread */
  def onFailureUi[U](pf: PartialFunction[Throwable, Ui[U]]) =
    future.onFailure(partialApplyUi(pf))(UiThreadExecutionContext)

  /** Same as onComplete, but performed on the UI thread */
  def onCompleteUi[U](pf: PartialFunction[Try[T], Ui[U]]) =
    future.onComplete(partialApplyUi(pf))(UiThreadExecutionContext)
}
