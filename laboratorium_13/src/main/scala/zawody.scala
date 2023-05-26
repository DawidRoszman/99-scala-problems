package jp1.akka.lab13

// „Interfejs użytkownika” wymaga pewnych dodatkowych elementów:
import scala.concurrent.ExecutionContext
import scala.util.control.Breaks._
import scala.io.StdIn

import akka.actor.{ActorSystem, Props}

import model.*

@main
def zawody: Unit = {


  val system = ActorSystem("system")
  val organizator = system.actorOf(Props[Organizator](), "organizator")

  // Interfejs „organizatora”:
  implicit val ec: ExecutionContext = ExecutionContext.global


  breakable {
    while (true) {
      StdIn.readLine("polecenie: ") match {
        case "start" =>
          // początek zawodów
          organizator ! Organizator.Start
        case "eliminacje" =>
          // polecenie rozegrania rundy eliminacyjnej
          organizator ! Organizator.Runda
        case "finał" =>
          organizator ! Organizator.StartFinals
          organizator ! Organizator.Runda
          // polecenie rozegrania rundy finałowej
          // wymaga zamknięcia Rundy eliminacyjnej i utworzenie
          // Rundy finałowej, zawierającej najlepszych 20.
          // zawodników z Rundy eliminacyjnej
        case "wyniki" =>
          organizator ! Organizator.Wyniki
          // żądanie rankingów (lub rankingu finałowego)
        case "stop" =>
          organizator ! Organizator.Stop
          break()
        case _ =>
      }
    }
  }

}
