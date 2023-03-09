// typy argumentów są obowiązkowe, co zwraca funkcja niekoniecznie - scala sobie sama sprawdzi, ale powinniśmy ją dawać
def inc1(n: Int) = {
  n + 1 // :Int
  println(n + 1) // () :Unit
  // funkcja zwraca to co jest ostatnie
}

// powinniśmy jawnie pokazywać co funkcja zwraca
def inc2(n: Int): Int = {
  n + 2
}

// funkcję można również zapisać w taki sposób, jeśli mamy tylko jedno wyrażenie
def inc2a(n: Int): Int = n + 2

lazy val dziwne = {
  val x = 123
  x + 1 // :Int
  println(x + 1) // () :Unit
  x.toString // konwersja do stringa
  x
}

// Nie trzeba pisać, że w1 to String, kompilator sam to ogarnie
// val w1: String = if (dziwne > 100) "OK" else "nie OK" // wyrażenie warunkowe, a nie instrukcja warunkowa

// Tutaj mamy typ zmiennej Any - ogólny typ (cokolwiek)
// val w2 = if (dziwne > 100) "OK" else -1
// Czyli mamy to samo co to:
// val w2: Any = if (dziwne > 100) "OK" else -1

// Powstaje problem przepełnienia stostu, więc możemy ją ulepszyć
def silnia(n: Int): Int = {
  if (n <= 0) 1
  else n * silnia(n - 1)
  // ten problem się buduje przez mnożenie w tym działaniu, bo "odkładamy je na później"
}

def silnia2(n: Int): Int = {
  n match {
    case 0 => 1
    case _ => n * silnia2(n - 1)
  }
}
def fibomatch(n: Int): Int =
  n match {
    case 0 | 1 => 1
    case _ => fibomatch(n - 1) + fibomatch(n - 2)
  }

def fibTailRec(n: Int): Int = {
  @annotation.tailrec // adnotacja, która sprawdza czy funkcja jest ogonowa
  def go(n: Int, a: Int, b: Int): Int = n match
    case 0 => a
    case 1 => b
    case _ => go(n - 1, b, a + b)
  go(n, 0, 1)
}

@main
def mainSilnia: Unit = {
  val wynik = fibomatch(5)
  println(wynik)
}

def funDomyslna(n: Int = 1): Unit = println("#" * n)

// Żeby to zoptymalizować do funkcji będziemy podawać zarówno liczbę jak i to przez ile chcemy pomnożyć
// Rekurencję można wykonywać ogonowo, czyli dochodzimy do końca i zwracamy wynik, a nie że musimy rozwijać ją z powrotem od końca

// def lepszaSilnia(n: Int, akumulator: Int = 1): Int = {
//   n match
//     case 1 => akumulator
//     case _ => lepszaSilnia(n - 1, akumulator * n)
//   }
// }

// def lepszaSilnia2(n: Int, akumulator: Int = 1): Int = {
//   @annotation.tailrec
//   def go(n: Int, akumulator: Int): Int = n match {
//     case 1 => akumulator
//     case _ => go(n - 1, akumulator * n)
//   }
//   go(n, akumulator)
// }

@main def mainSilniaTail: Unit = {
  val wynik = lepszaSilnia2(5)
  println(wynik)
}
// ??? - wyrażenie odpowiedniego typu, które oznacza, że to jest coś co dopiero będzie zdefiniowane

def fibo(n: Int): Int = {
  if (n == 0 || n == 1) 1
  else fibo(n - 1) + fibo(n - 2)
}

@main def mainProg: Unit = {
  // val wynik = inc1(123)
  // println(wynik)
  // println(dziwne)

  // val wynik2 = silnia(100) // wyszliśmy poza zakres Int, więc wynik = 0, blisko granicy, wyniki mogą wychodzić różne
  // val wynik2 = silnia(100000) // stackOverflowError
  // val wynik2 = silnia(5)
  // println(wynik2)

  funDomyslna() // Domyślne - 1
  funDomyslna(20)
  funDomyslna(n = 40) // Można go też jawnie podać

  val wynik3 = fibo(10)
  println(wynik3)
}
