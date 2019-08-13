import cats.Monoid
import cats.implicits._
import cats.kernel.CommutativeMonoid

trait Fold[I, O] {
  type M

  def tally(i: I): M

  def summarize(m: M): O

  def monoid: Monoid[M]
}

object Fold {
  def runList[I, O](fold: Fold[I, O], data: List[I]) = {
    fold.summarize(
      data.map(i => fold.tally(i))
        .foldRight(fold.monoid.empty)((x, y) => fold.monoid.combine(x, y)) // or combineAll
    )
  }
}

object sumfold {
  val sum = new Fold[Int, Int] {
    type M = Int;

    def tally(i: M) = i

    def summarize(sum: M) = sum

    def monoid = implicitly //implict for int is adding with zero element being 0
  }
}

object sumWords {
  val sumWords = new Fold[String, Map[String, Int]] {
    type M = Map[String, Int]

    def tally(i: String) = Map(i -> 1)

    def summarize(sum: M) = sum

    def monoid = implicitly //implicit for Map is merging 2 maps
  }
}

object average {
  val Average = new Fold[Int, Option[Int]] {
    type M = Tuple2[Int, Int]

    def tally(i: Int) = Tuple2(i, 1);

    def summarize(sum: M) = if (sum._2 == 0) Option.empty else Option.apply(sum._1 / sum._2)

    def monoid = implicitly // it just add tuples
  }
}


object max {
  val max = new Fold[Int, Option[Int]] {
    type M = Int

    def tally(i: Int) = i

    def summarize(sum: M) = if (sum == Int.MinValue) Option.empty else Option.apply(sum)

    def monoid = new CommutativeMonoid[Int]() {
      override def empty = Int.MinValue

      override def combine(x: Int, y: Int): Int = if (x > y) x else y
    }
  }
}


object Unfold {

  def unfold[A, B](initial: A)(fun: A => Option[(A, B)]): Stream[B] = {
    fun(initial).map { case (a, b) => b #:: unfold(a)(fun) }.getOrElse(Stream.empty)
  }


  def unfold0[A](initial: A)(fun: A => Option[A]): Stream[A] = {
    unfold(initial)({ st => fun(st).map({ x => (x, x) }) })
  }


  //I do not get it
  def unfoldE[A, B](initial: A)(fun: A => Stream[Either[A, B]]): Stream[B] =
    fun(initial).flatMap {
      case Left(a) => unfoldE(a)(fun)
      case Right(b) => Stream(b)
    }

  def leapYears(from: Int): Stream[Int] = unfold0(from)(x => Some(x + 1)).filter({ year =>
    (year % 4 == 0) && (!(year % 100 == 0) || (year % 400 == 0))
  })

  def hailstone(from: Int): Stream[Int] = unfold0(from)(x =>
    if (x == 1) None
    else {
      if (x % 2 == 0) {
        Some(x / 2)
      } else {
        Some(x * 3 + 1)
      }
    }
  )

  def fib: Stream[Int] = unfold((1, 1))(x => {
    val i = x._1 + x._2
    Some((x._2, i), x._1)
  }
  )

  val fibonacci: Stream[Int] = unfold((1, 1))({
    case (prev, current) =>
      Some(((current, prev + current), prev))
  })
}


object start {
  def main(args: Array[String]): Unit = {
    val i = Fold.runList(sumfold.sum, List(1, 2, 4))
    println(i)


    val ss = Fold.runList(sumWords.sumWords, List("a", "v", "x", "v"))
    println(ss)


    val xx = Fold.runList(average.Average, List(1, 2, 4))
    println(xx)

    val m = Fold.runList(max.max, List(1, 2, 4, 2))
    println(m)


    val ints = Unfold.leapYears(1990).take(3).toList
    println(ints);

    val haild = Unfold.hailstone(12).toList
    println(haild);


    val fib = Unfold.fib.take(5).toList
    println(fib);
  }
}