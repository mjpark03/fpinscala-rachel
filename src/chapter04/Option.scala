

sealed trait Option[+A] {
  
  /*
    exercise 4.1
  */
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  
  def mean(seq: Seq[Double]): Option[Double] = {
    if(seq.isEmpty) None
    else Some(seq.sum / seq.length)
  }
}
