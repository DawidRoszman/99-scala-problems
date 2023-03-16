// The Option type in Scala is a container for zero or one element of a given type. It can either be Some[T] or None object, which represents a missing value1. It is used to describe a computation that either has a result or does not2. For example, when a method returns a value that can even be null then Option is utilized i.e., the method defined returns an instance of an Option instead of returning an object or null3.
@main
def learning3(): Unit =
  val lista = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
  val smth = rotate(3, lista)
  val smth2 = rotate(-2, lista)
  println(s"es $smth")
  println(s"es $smth2")
// imie required
// nazwisko required
// numer tel optional Option[String] = NONE or Some[String] = null
//  tel match {
//   case Some => println("tel")
//   case None => println("brak")
// }
def sumuj(l: List[Option[Double]]): Option[Double] = {
  @annotation.tailrec
  def pomocnicza(l: List[Option[Double]], acc: Option[Double]): Option[Double] = {
    l match {
      case Some(x) :: tail if x > 0 => pomocnicza(l.tail, Some(acc.get + x))
      case Some(x) :: tail if x <= 0 => pomocnicza(l.tail, acc)
      case None :: tail => pomocnicza(l.tail, acc)
      case _ if acc == Some(0.0) => None
      case _ => acc
    }
  }
  pomocnicza(l, Some(0.0))
}

def tasuj(l1: List[Int], l2: List[Int]): List[Int] = {
  def go[A](l1: List[Int], l2: List[Int], aku: List[Int] = Nil): List[Int] = {
    (l1, l2) match {
      case (Nil, Nil) => aku

      case (head :: tail, head2 :: tail2) if (head >= head2 && ostatni(aku) != head2) =>
        go(l1, tail2, aku :+ head2)
      case (head :: tail, head2 :: tail2) if (head < head2 && ostatni(aku) != head) =>
        go(tail, l2, aku :+ head)

      case (head :: tail, head2 :: tail2) if (head >= head2 && ostatni(aku) == head2) =>
        go(l1, tail2, aku)
      case (head :: tail, head2 :: tail2) if (head < head2 && ostatni(aku) == head) =>
        go(tail, l2, aku)

      case (head :: tail, Nil) if (ostatni(aku) != head) => go(tail, Nil, aku :+ head)
      case (Nil, head2 :: tail2) if (ostatni(aku) != head2) => go(Nil, tail2, aku :+ head2)

      case (head :: tail, Nil) if (ostatni(aku) == head) => go(tail, Nil, aku)
      case (Nil, head2 :: tail2) if (ostatni(aku) == head2) => go(Nil, tail2, aku)

      case _ => aku
    }
  }
  go(l1, l2)
}

def ostatni(list: List[Int]): Int = {
  list match {
    case Nil => 2137 // blad💩
    case head :: Nil => head
    case _ :: tail => ostatni(tail)
  }
}

// def tasuj(l1: List[Int], l2: List[Int]): List[Int] = {
//   @annotation.tailrec
//   def go(l1: List[Int], l2: List[Int], acc: List[Int]): List[Int] = {
//     (l1, l2) match {
//       case (Nil, Nil) => acc
//       case (Nil, _) => acc ++ l2
//       case (_, Nil) => acc ++ l1
//       case (h1 :: t1, h2 :: t2) => {
//         if (h1 < h2) go(t1, l2, acc :+ h1)
//         else if (h1 == h2) go(t1, t2, acc :+ h1)
//         else go(l1, t2, acc :+ h2)
//       }
//     }
//   }
//   def helper(l: List[Int]): List[Int] = {
//     def go(l: List[Int], acc: List[Int]): List[Int] = {
//       l match {
//         case Nil => acc
//         case head :: tail => go(tail.dropWhile(_ == head), acc :+ head)
//       }
//     }
//     go(l, List())
//   }
//   helper(go(l1, l2, List()))
// }

// case (Nil, Nil) => aku: This case matches when both input lists are empty. In this case, the function returns the accumulator list aku as the final result.

// case (head :: tail, head2 :: tail2) if (head >= head2 && ostatni(aku) != head2): This case matches when both input lists are non-empty and the first element of l1 is greater than or equal to the first element of l2, and the last element added to the accumulator list is not equal to the first element of l2. In this case, the function adds the first element of l2 to the accumulator list and calls itself recursively with updated arguments.

// case (head :: tail, head2 :: tail2) if (head < head2 && ostatni(aku) != head): This case matches when both input lists are non-empty and the first element of l1 is less than the first element of l2, and the last element added to the accumulator list is not equal to the first element of l1. In this case, the function adds the first element of l1 to the accumulator list and calls itself recursively with updated arguments.

// case (head :: tail, head2 :: tail2) if (head >= head2 && ostatni(aku) == head2): This case matches when both input lists are non-empty and the first element of l1 is greater than or equal to the first element of l2, and the last element added to the accumulator list is equal to the first element of l2. In this case, the function calls itself recursively with updated arguments without adding any new elements to the accumulator list.

// case (head :: tail, head2 :: tail2) if (head < head2 && ostatni(aku) == head): This case matches when both input lists are non-empty and the first element of l1 is less than the first element of l2, and the last element added to the accumulator list is equal to the first element of l1. In this case, the function calls itself recursively with updated arguments without adding any new elements to the accumulator list.

// case (_, Nil) => go(l1.tail, l2, aku :+ l1.head): This case matches when only one input list is empty. In this case, it adds an element from non-empty list into accumulator and calls itself recursively with updated arguments.

// case (Nil, _) => go(l1.tail, l2.tail, aku :+ l1.head :+ l2.head): This case matches when only one input list is empty. In this case it adds an element from non-empty list into accumulator and calls itself recursively with updated arguments.

// Examples:

// scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
// res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)

// scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
// res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
def P19(list: List[Char], n: Int): List[Char] = {
  def go(list: List[Char], n: Int, aku: List[Char]): List[Char] = {
    (list, n) match {
      case (_, 0) => list ::: aku
      case (head :: tail, n) =>
        if n > 0 then go(tail, n - 1, aku :+ head) else go(tail, n + 1, aku :+ head)
      case (Nil, _) => throw new Exception
    }
  }
  go(list, n, Nil)
}

def P19_2(list: List[Char], n: Int): List[Char] = {

  def go(list: List[Char], n: Int, aku: List[Char]): List[Char] = {

    (list, n) match {
      case (_, 0) => list ::: aku
      case (head :: tail, n) =>
        if n > 0 then go(tail, n - 1, aku :+ head) else go(tail, n + 1, head :: aku)
      case (Nil, _) => throw new Exception
    }
  }
  if n > 0 then go(list, n, Nil) else go(list.reverse, -n, Nil).reverse
}

def rotate[A](n: Int, xs: List[A]): List[A] = {
  def go(n: Int, xs: List[A], acc: List[A]): List[A] = (n, xs) match {
    case (_, Nil) => acc
    case (0, _) => xs ::: acc.reverse
    case (n, x :: tail) if n > 0 => go(n - 1, tail, x :: acc)
    case (n, _) if n < 0 => go(n + 1, acc.head :: xs, acc.tail)
  }

  val nBounded = if (xs.isEmpty) 0 else n % xs.length
  go(nBounded, xs, Nil)
}
