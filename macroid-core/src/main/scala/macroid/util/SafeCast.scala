package macroid.util

import scala.util.Try

/*

try to case from -> to

SafeCast[from, to](None) -> None
SafeCast[from, to]("fs") -> fs
 */
object SafeCast {
  def apply[From, To](x: From) = Try(Option(x).map(_.asInstanceOf[To])).toOption.flatten
}
