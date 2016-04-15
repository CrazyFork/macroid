package macroid

import android.view.animation.{ AlphaAnimation, Animation }
import android.view.View
import scala.concurrent.{ Future, Promise, ExecutionContext }
import android.view.animation.Animation.AnimationListener
import scala.util.Success
import android.widget.ProgressBar
import java.util.concurrent.{ TimeUnit, Executors }
import scala.util.control.NonFatal

private[macroid] object SnailScheduler {
  val scheduler = Executors.newScheduledThreadPool(1) // all animation will reuse this thread
  def snailSchedulerEc(millis: Long) = new ExecutionContext {
    def execute(runnable: Runnable) = scheduler.schedule(runnable, millis, TimeUnit.MILLISECONDS)

    def reportFailure(t: Throwable) = t.printStackTrace()
  }
}

//discard result, convert future to new with recover abilities but discard f's result
private[macroid] object AfterFuture {
  def apply[A](f: Future[A])(implicit ec: ExecutionContext): Future[Unit] =
    f.map(_ ⇒ ()) recover { case NonFatal(_) ⇒ () } //recover on non-fatal error
}

private[macroid] trait BasicSnails {
  import SnailScheduler._

  /** A delay to be inserted somewhere between <~~s and <~s */
  // A delay snail for x milisecs
  def delay(millis: Long) = Snail[View](x ⇒ Future(())(snailSchedulerEc(millis)))

  /** A snail that waits for a given future to finish */
  def wait(f: Future[Any])(implicit ec: ExecutionContext) = Snail[View](x ⇒ AfterFuture(f))
}

// utilities for control progress bar
private[macroid] trait ProgressSnails extends BasicSnails with VisibilityTweaks {
  /** Show this progress bar with indeterminate progress and hide it once `future` is done */
  def waitProgress(future: Future[Any])(implicit ec: ExecutionContext): Snail[ProgressBar] =
    Tweak[ProgressBar](_.setIndeterminate(true)) + show ++ wait(future) + hide // hide wont be call if wait(future) failed

  /** Show this progress bar with determinate progress and hide it once all `futures` are done */
  def waitProgress(futures: List[Future[Any]])(implicit ec: ExecutionContext): Snail[ProgressBar] =
    Tweak[ProgressBar] { x ⇒
      x.setIndeterminate(false)
      x.setMax(futures.length)
      x.setProgress(0)
      futures.foreach(f ⇒ AfterFuture(f).foreachUi(_ ⇒ Ui(x.incrementProgressBy(1))))
    } + show ++ wait(Future.sequence(futures)) + hide
}

private[macroid] trait AnimationSnails extends VisibilityTweaks {
  /** Run animation, indicating when it’s finished */
  def anim(animation: Animation, duration: Long = -1L) = Snail[View] { x ⇒
    val animPromise = Promise[Unit]()
    animation.setAnimationListener(new AnimationListener {
      override def onAnimationStart(a: Animation) = {}
      override def onAnimationRepeat(a: Animation) = {}
      override def onAnimationEnd(a: Animation) = { animPromise.complete(Success(())) }
    })
    if (duration >= 0) animation.setDuration(duration)
    x.startAnimation(animation)
    animPromise.future
  }

  /** Fade in this view */
  def fadeIn(millis: Long) = show ++ anim(new AlphaAnimation(0, 1), duration = millis)
  /** Fade out this view */
  def fadeOut(millis: Long) = anim(new AlphaAnimation(1, 0), duration = millis) + hide
}

private[macroid] trait Snails
  extends BasicSnails
  with ProgressSnails
  with AnimationSnails

object Snails extends Snails
