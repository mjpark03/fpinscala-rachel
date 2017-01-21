
sealed trait Option[+A] {
  
  /*
    exercise 4.1
  */
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def orElse[B>:A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob
  
  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(v) if f(v) => this
  }

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  
  def mean(seq: Seq[Double]): Option[Double] = {
    if(seq.isEmpty) None
    else Some(seq.sum / seq.length)
  }

  def variance(seq: Seq[Double]): Option[Double] = {
    mean(seq).flatMap(m => mean(seq.map(x => math.pow(x - m, 2))))
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap (aa => b map (bb => f(aa, bb)))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }
  }

  def sequence_1[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))
  }

}
