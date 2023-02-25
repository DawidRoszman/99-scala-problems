import scala.annotation.tailrec

@main def P01: Unit =
  def find_last[A](ls: List[A]): A = ls match
    case head :: Nil => head
    case _ :: tail => find_last(tail)
    case _ => throw new NoSuchElementException

@main def P02: Unit =
  def find_last_but_one[A](ls: List[A]): A = ls match
    case head :: _ :: Nil => head
    case _ :: tail => find_last_but_one(tail)
    case _ => throw new NoSuchElementException

@main def P03: Unit =
  def nth[A](n: Int, ls: List[A]): A = (n, ls) match
    case (0, head :: _) => head
    case (n, _ :: tail) => nth(n - 1, tail)
    case (_, Nil) => throw new NoSuchElementException

@main def P04: Unit =
  def length[A](ls: List[A]): Int = ls match
    case Nil => 0
    case _ :: tail => 1 + length(tail)

@main def P05: Unit =
  def reverse[A](ls: List[A]): List[A] = ls match
    case Nil => Nil
    case head :: tail => reverse(tail) ::: List(head)

@main def P06: Unit =
  def isPalindrome[A](ls: List[A]): Boolean = ls == ls.reverse

@main def P07: Unit =
  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case ms: List[_] => flatten(ms)
    case e => List(e)
  }

@main def P08: Unit =
  def compressTailRecursive[A](ls: List[A]): List[A] = 
    @tailrec
    def compressR(result: List[A], curList: List[A]): List[A] = curList match 
      case head :: tail => compressR(head :: result, tail.dropWhile(_ == head))
      case Nil       => result.reverse
    
    compressR(Nil, ls)
  val l = List('a', 'b', 'c', 'c', 'a', 'a')
  println(compressTailRecursive(l))

