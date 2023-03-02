val arr = List(2, 23, -23, 12, 55, 7, -8, 5, 9, -54)
val arr2 = List(1, 2, 2, 1)

@main
def Main(): Unit = {
  println(Erastotenes(30))
}

//P01 (*) Find the last element of a list.
// head (mozna uzyc innego slowa) - pierwszy element
// tail (mozna uzyc innego slowa) - reszta listy
// head :: tail - lista złożona z heada i taila
// Nil - pusta lista
// lastElement[A] - funkcja, która przyjmuje listę elementów typu A i zwraca element typu A
//Krok 1: head = 2, tail = [23, -23, 12, 55, 7, -8, 5, 9, -54]
//_ :: tail
// head = -54 tail = []
//Krok 2: head = 23 tail = [-23, 12, 55, 7, -8, 5, 9, -54]
//Krok 3: head = -23 tail = [12, 55, 7, -8, 5, 9, -54]
//Krok 4: head = 12 tail = [55, 7, -8, 5, 9, -54]
//Krok 5: head = 55 tail = [7, -8, 5, 9, -54]
//Krok 6: head = 7 tail = [-8, 5, 9, -54]
//Krok 7: head = -8 tail = [5, 9, -54]
//Krok 8: head = 5 tail = [9, -54]
//Krok 9: head = 9 tail = [-54]
//Krok 10: head = -54 tail = []
// head :: head2 :: tail - lista złożona z heada i head2 i taila
//A - any type
// Nil == []
def lastElement[A](list: List[A]): A = {
  list match {
    case head :: Nil => head
    case _ :: tail => lastElement(tail)
    case _ => throw new NoSuchElementException
  }
}
//PO2 (*) Find the last but one element of a list.

def lastElement2[A](list: List[A]): A = {
  list match {
    case head :: _ :: Nil => head
    case _ :: tail => lastElement2(tail)
    case _ => throw new NoSuchElementException
  }
}

//P03 (*) Find the Kth element of a list.

def kthElement[A](list: List[A], n: Int): A = {
  (list, n) match {
    case (head :: _, 0) => head
    case (_ :: tail, n) => kthElement(tail, n - 1)
    case (Nil, _) => throw new NoSuchElementException
  }
}

//P05 (*) Reverse a list.
def reverseTailRecursive[A](ls: List[A]): List[A] = {
  def reverseR(result: List[A], curList: List[A]): List[A] =
    curList match {
      case Nil => result
      case h :: tail => reverseR(h :: result, tail)
    }
  reverseR(Nil, ls)
}
// h :: result - lista złożona z heada i resulta (head :: result)
// Krok 1: result = [], curList = [2, 23, -23, 12, 55, 7, -8, 5, 9, -54]
// head = 2, tail = [23, -23, 12, 55, 7, -8, 5, 9, -54]
//Krok 2: result = [2], curList = [23, -23, 12, 55, 7, -8, 5, 9, -54]
// head = 23, tail = [-23, 12, 55, 7, -8, 5, 9, -54]
//Krok 3: result = [23, 2], curList = [-23, 12, 55, 7, -8, 5, 9, -54]
//Krok 4: result = [-23, 23, 2], curList = [12, 55, 7, -8, 5, 9, -54]
//Krok 5: result = [12, -23, 23, 2], curList = [55, 7, -8, 5, 9, -54]
//Krok 6: result = [55, 12, -23, 23, 2], curList = [7, -8, 5, 9, -54]
//Krok 7: result = [7, 55, 12, -23, 23, 2], curList = [-8, 5, 9, -54]
//Krok 8: result = [-8, 7, 55, 12, -23, 23, 2], curList = [5, 9, -54]
//Krok 9: result = [5, -8, 7, 55, 12, -23, 23, 2], curList = [9, -54]
//Krok 10: result = [9, 5, -8, 7, 55, 12, -23, 23, 2], curList = [-54]
//Krok 11: result = [-54, 9, 5, -8, 7, 55, 12, -23, 23, 2], curList = []
//Zwracamy result

//P06 (*) Find out whether a list is a palindrome.
def listPalindrome[A](list: List[A]): Boolean = {
  val reversedList = reverseTailRecursive(list)
  if list == reversedList then true else false
}
// if(n == 9){ return 9}
// else if(n == 2) { return 2}
// else { return 0}
// n match {
//   case 9 => 9
//   case 2 => 2
//   case _ => 0
// }
//P32 (*) Determine the greatest common divisor of two positive integer numbers.
def NWD(a: Int, b: Int): Int = if (a == 0) then b else NWD(b % a, a)
//P39 (*) A list of prime numbers.

def Erastotenes(n: Int): List[Int] = {
  val list = (2 to n).toList
  def go(n: Int, akumulator: List[Int], ogList: List[Int]): List[Int] = {
    (ogList, akumulator) match {
      case (Nil, _) => akumulator
      case (head :: tail, _) => go(n, akumulator.appended(head), tail.filter(_ % head != 0))
    }
  }
  go(n, List(), list)
}
// Krok 1: n = 10, akumulator = [], ogList = [2, 3, 4, 5, 6, 7, 8, 9, 10]
// head = 2, tail = [3, 4, 5, 6, 7, 8, 9, 10]
// Krok 2: n = 10, akumulator = [2], ogList = [3, 4, 5, 6, 7, 8, 9, 10]
// head = 3, tail = [4, 5, 6, 7, 8, 9, 10]
// Krok 3: n = 10, akumulator = [2, 3], ogList = [4, 5, 6, 7, 8, 9, 10]
// head = 4, tail = [5, 6, 7, 8, 9, 10]
// Krok 4: n = 10, akumulator = [2, 3, 5], ogList = [6, 7, 8, 9, 10]
// head = 6, tail = [7, 8, 9, 10]
// Krok 5: n = 10, akumulator = [2, 3, 5, 7], ogList = [8, 9, 10]
// head = 8, tail = [9, 10]
// Krok 6: n = 10, akumulator = [2, 3, 5, 7, 9], ogList = [10]
// head = 10, tail = []
// Krok 7: n = 10, akumulator = [2, 3, 5, 7, 9, 10], ogList = []
//Zwracamy akumulator

def lepszaSilnia2(n: Int, akumulator: Int = 1): Int = {
  @annotation.tailrec
  def go(n: Int, akumulator: Int): Int = n match {
    case 1 => akumulator
    case _ => go(n - 1, akumulator * n)
  }
  go(n, akumulator)
}
