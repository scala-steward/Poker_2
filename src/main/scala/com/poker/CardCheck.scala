package com.poker

import scala.util.Try
import scala.util.matching.Regex

class CardCheck(card: String) {
  //println(card +"   000")
  require(isValid(card), "Invalid card specification")

  def face: String = {
    // println("1")
    card.substring(0, 1)
  }

  def suit: String = {
    //println("2")
    card.substring(1, 2)
  }

  def value: Int = {
    //println("3")
    Card.value(face)
  }

  override def toString: String = {
    //println("4")
    card
  }

  private def isValid(card: String): Boolean = {
    //println("8")
    var card2 = card
    Try {
      val pattern = "([0-9]+)([A-Za-z])".r
      val pattern(num, str) = card2
      if (num == "10") {
        card2 = "T" + str
      }
    }

    new Regex("[2-9KTQJA]{1}[CDSH]{1}").findAllIn(card2).length > 0
  }
}

object Card {
  //println("5")
  def apply(cs: String): CardCheck = new CardCheck(cs)

  val suits: List[String] = List("C", "H", "D", "S")

  val faces: List[String] = (2 to 9).map(_.toString).toList ::: List("T", "J", "Q", "K", "A")

  def value(key: String): Int = {
    values.find(v => v._1 == key).get._2
  }

  private val values: List[(String, Int)] = List(
    ("2", 2),
    ("3", 3),
    ("4", 4),
    ("5", 5),
    ("6", 6),
    ("7", 7),
    ("8", 8),
    ("9", 9),
    ("T", 10),
    ("J", 11),
    ("Q", 12),
    ("K", 13),
    ("A", 14)
  )
}