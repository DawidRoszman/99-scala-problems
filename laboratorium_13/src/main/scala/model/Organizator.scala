package jp1.akka.lab13.model

import akka.actor.{Actor, ActorRef, ActorLogging, Props}

val akkaPathAllowedChars = ('a' to 'z').toSet union
  ('A' to 'Z').toSet union
  "-_.*$+:@&=,!~';.)".toSet

object Organizator {
  case object Start
  // rozpoczynamy zawody – losujemy 50 osób, tworzymy z nich zawodników
  // i grupę eliminacyjną
  case object Runda
  // polecenie rozegrania rundy (kwalifikacyjnej bądź finałowej) –  wysyłamy Grupa.Runda
  // do aktualnej grupy
  case object Wyniki
  // polecenie wyświetlenia klasyfikacji dla aktualnej grupy
  case class Wyniki(w: Map[ActorRef, Option[Ocena]])
  // wyniki zwracane przez Grupę
  case object Stop
  // kończymy działanie
  case object StartFinals
}

class Organizator extends Actor with ActorLogging {
  // importujemy komunikaty na które ma reagować Organizator
  import Organizator._

  def receive: Receive = {
    case Start =>
      // tworzenie 50. osób, opdowiadających im Zawodników
      // oraz Grupy eliminacyjnej
      val zawodnicy = List.fill(50) {
        val o = Utl.osoba()
        context.actorOf(
          Props(Zawodnik(o)),
          s"${o.imie}-${o.nazwisko}" filter akkaPathAllowedChars
        )
      }
      val grupa = context.actorOf(Props(Grupa(zawodnicy)), "grupa")
      context.become(ready(grupa, Nil))

    // Obsługa pozostałych komunikatów

    case Stop =>
      log.info("kończymy zawody...")
      context.system.terminate()
  }
  def ready(
      grupa: ActorRef,
      wyniki: List[(ActorRef, Option[Ocena])]
  ): Receive = {
    case Stop =>
      log.info("kończymy zawody...")
      context.system.terminate()
    case Runda =>
      grupa ! Grupa.Runda
    case Wyniki(w) =>
      val wyn = w
        .filter((_, wynik) =>
          wynik match {
            case Some(_) => true
            case None    => false
          }
        )
        .toList
        .sortWith((a, b) =>
          // a._2.get.nota1 + a._2.get.nota2 + a._2.get.nota3 >
          //   b._2.get.nota1 + b._2.get.nota2 + b._2.get.nota3
          val aWynik = List(a._2.get.nota1, a._2.get.nota2, a._2.get.nota3)
          val bWynik = List(b._2.get.nota1, b._2.get.nota2, b._2.get.nota3)

          if aWynik.sum > bWynik.sum then false
          else if aWynik.sum == bWynik.sum then
            if aWynik(0) > bWynik(0) then false
            else if aWynik(0) == bWynik(0) && aWynik(2) > bWynik(2) then false
            else true
          else true
        )
        .reverse
      context.become(ready(grupa, wyn))
    case Wyniki =>
      // val withIndex = wyniki.toList.zipWithIndex
      // withIndex.foreach((wynik, index) =>
      //   log.info(
      //     s"${index + 1}. ${wynik._1.path.name} ${wynik._2.get.nota1} ${wynik._2.get.nota2} ${wynik._2.get.nota3}, suma = ${wynik._2.get.nota1 + wynik._2.get.nota2 + wynik._2.get.nota3}}}"
      //   )
      // )
      // case Wyniki =>
      //   val aaaaaaaaaaaaaaaaaaaaaaa = wyniki.groupBy(_._2)
      //   .toList
      //   .map(x => (wyniki.filter(_._2)))
      val res = wyniki.map(x =>
        (
          wyniki
            .filter(el =>
              val nota1_el = el._2.get.nota1
              val nota2_el = el._2.get.nota2
              val nota3_el = el._2.get.nota3
              val el_suma = nota1_el + nota3_el + nota3_el

              val nota1_x = x._2.get.nota1
              val nota2_x = x._2.get.nota2
              val nota3_x = x._2.get.nota3
              val x_suma = nota1_x + nota2_x + nota3_x
              x_suma < el_suma || (el_suma == x_suma && nota1_el < nota1_x) ||
              (el_suma == x_suma && nota1_el == nota1_x && nota3_el < nota3_x)
            )
            .length + 1,
          x._1.path.name,
          x._2.get.nota1,
          x._2.get.nota2,
          x._2.get.nota3,
          x._2.get.nota1 + x._2.get.nota2 + x._2.get.nota3
        )
      )
      res.foreach(x =>
        log.info(
          s"${x._1}. ${x._2} ${x._3} ${x._4} ${x._5}, suma = ${x._6}"
        )
      )
    case StartFinals =>
      val grupa2 =
        context.actorOf(
          Props(Grupa(wyniki.map(_._1).toList.take(5))),
          "grupa2"
        )
      context.become(finall(wyniki.toMap, grupa2))
  }
  def finall(wyniki: Map[ActorRef, Option[Ocena]], grupa: ActorRef): Receive = {
    case Stop =>
      log.info("kończymy zawody...")
      context.system.terminate()
    case Runda =>
      grupa ! Grupa.Runda
    case Wyniki(w) =>
      val w2 = w
        .filter((_, wynik) =>
          wynik match {
            case Some(_) => true
            case None    => false
          }
        )

      val wyniki_finalowe = w2
        .map((key, value) =>
          if wyniki.exists(wynik => wynik._1 == key) then
            val wynik2 = wyniki.find(wynik => wynik._1 == key)
            (key, List(wynik2.get._2, value))
          else (key, List(value))
        )
        .map(z =>
          z._1 ->
            z._2.foldLeft(Option(Ocena(0, 0, 0)))((acc, currVal) =>
              val oc1 = acc.get.nota1 + currVal.get.nota1
              val oc2 = acc.get.nota2 + currVal.get.nota2
              val oc3 = acc.get.nota3 + currVal.get.nota3
              Option(Ocena(oc1, oc2, oc3))
            )
        )
        .toList
        .sortWith((a, b) =>
          val aWynik = List(a._2.get.nota1, a._2.get.nota2, a._2.get.nota3)
          val bWynik = List(b._2.get.nota1, b._2.get.nota2, b._2.get.nota3)
          if aWynik.sum < bWynik.sum then true
          else if aWynik.sum == bWynik.sum then
            if aWynik(0) < bWynik(0) then true
            else if aWynik(0) == bWynik(0) && aWynik(2) < bWynik(2) then true
            else false
          else false
        )
        .reverse
      context.become(wyniki_kurwa_gotowe(wyniki_finalowe))

  }
  def wyniki_kurwa_gotowe(wyniki: List[(ActorRef, Option[Ocena])]): Receive = {
    case Wyniki =>
      // val withIndex = wyniki.toList.zipWithIndex
      // withIndex.foreach((wynik, index) =>
      //   log.info(
      //     s"${index + 1}. ${wynik._1.path.name} ${wynik._2.get.nota1} ${wynik._2.get.nota2} ${wynik._2.get.nota3}, suma = ${wynik._2.get.nota1 + wynik._2.get.nota2 + wynik._2.get.nota3}}}"
      //   )
      // )
      // case Wyniki =>
      //   val aaaaaaaaaaaaaaaaaaaaaaa = wyniki.groupBy(_._2)
      //   .toList
      //   .map(x => (wyniki.filter(_._2)))
      val res = wyniki.map(x =>
        (
          wyniki
            .filter(el =>
              val nota1_el = el._2.get.nota1
              val nota2_el = el._2.get.nota2
              val nota3_el = el._2.get.nota3
              val el_suma = nota1_el + nota3_el + nota3_el

              val nota1_x = x._2.get.nota1
              val nota2_x = x._2.get.nota2
              val nota3_x = x._2.get.nota3
              val x_suma = nota1_x + nota2_x + nota3_x
              x_suma < el_suma 
              // || (el_suma == x_suma && nota1_el > nota1_x) 
              // || (el_suma == x_suma && nota1_el == nota1_x && nota3_el > nota3_x)
            ).map(_._1.path.name),
          x._1.path.name,
          x._2.get.nota1,
          x._2.get.nota2,
          x._2.get.nota3,
          x._2.get.nota1 + x._2.get.nota2 + x._2.get.nota3
        )
      )
      res.foreach(x =>
        log.info(
          s"${x._1}. ${x._2} ${x._3} ${x._4} ${x._5}, suma = ${x._6}"
        )
      )
  }
}
// val res = wyniki.map(x=> (wyniki.filter(el => x.suma < el.suma || (el.suma == x.suma && x.średniWdzięk < el.średniWdzięk)).length + 1,x.imię,x.nazwisko,x.suma))
// 🏳️🏳️🏳️🏳️🏳️🏳️🏳️🏳️🏳️🏳️🏳️🏳️🏳️🏳️🏳️🏳️🏳️🏳️🏳️🏳️🏳️🏳️🏳️🏳️🏳️🏳️🏳️🏳️🏳️🏳️🏳️🏳️🏳️🏳️🏳️