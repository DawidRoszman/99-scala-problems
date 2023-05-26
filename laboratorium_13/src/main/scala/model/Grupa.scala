package jp1.akka.lab13.model

import akka.actor.{Actor, ActorRef, ActorLogging}
import akka.actor.PoisonPill

object Grupa {
  case object Runda
  // Zawodnicy mają wykonać swoje próby – Grupa
  // kolejno (sekwencyjnie) informuje zawodników
  // o konieczności wykonania próby i „oczekuje”
  // na ich wynik (typu Option[Ocena])
  case object Wyniki
  // Polecenie zwrócenia aktualnego rankingu Grupy
  // Oczywiście klasyfikowani są jedynie Zawodnicy,
  // którzy pomyślnie ukończyli swoją próbę
  case class Wynik(ocena: Option[Ocena])
  // Informacja o wyniku Zawodnika (wysyłana przez Zawodnika do Grupy)
  // np. Wynik(Some(Ocena(10, 15, 14)))
  // Jeśli zawodnik nie ukończy próby zwracana jest wartość Wynik(None)
  case object Koniec
  // Grupa kończy rywalizację
}
class Grupa(zawodnicy: List[ActorRef]) extends Actor with ActorLogging {
  def receive: Receive = {
    case Grupa.Runda => 
      zawodnicy.foreach(zaw => zaw ! Zawodnik.Próba)
      context.become(scores(Map.empty))

    // case msg => log.info(s"msg")
  }
  def scores(w: Map[ActorRef, Option[Ocena]]): Receive = {
    case Grupa.Wynik(ocena) =>
      val m = w + (sender() -> ocena)
      context.become(scores(m))
      if m.size == zawodnicy.length then 
        context.parent ! Organizator.Wyniki(m)
        self ! PoisonPill
  }

}
