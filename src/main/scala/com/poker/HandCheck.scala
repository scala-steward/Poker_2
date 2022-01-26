package com.poker

import scala.util.Try
import Ordering.Implicits._


class HandCheck(val cards: List[CardCheck]) {
  require(isValid, "Hand cards are not valid")


  def compareTo(that: HandCheck): (Option[HandCheck], Option[HandType], String) = {
    def eval(h: HandCheck): (HandCheck, HandType, List[Int]) = {
      HandTypeList.all.collect { case t => t.evaluate(h) }.find { case (b, t, s) => b } match {
        case None => throw new Error("Not likely to happen!!")
        case Some(v) =>
          val hh = h.cards.map(x => Card(x.toString.replace("T", "10")))
          val h4 = HH(hh.map(x => x.toString))
          // println(h4, v._2, v._3)
          (h4, v._2, v._3)

      }
    }


    def compareBothHands(a: (HandCheck, HandType, List[Int]), b: (HandCheck, HandType, List[Int])): Option[(HandCheck, HandType, HandCheck)] = {

      (a._3 > b._3, b._3 > a._3) match {
        case (true, false) => Some((a._1, a._2, b._1))
        case (false, true) => Some((b._1, b._2, a._1))

        case _ => None
      }
    }

    compareBothHands(eval(this), eval(that)) match {
      case Some(v) => (Option(v._1), Option(v._2), s"Correct, hand [${v._1}] wins over [${v._3}]   //${v._2}")
      case None => (None, None, s"Incorrect, hand  ${eval(this)._2} wins over ")
    }
  }


  def pairs = group(2)

  def trips = group(3)

  def quads = group(4)

  def isSameSuit: Boolean = {
    cards.groupBy(_.suit).toList.length == 1
  }

  def isConsecutive: Boolean = {
    def get(swap: Boolean): List[Int] = swap match {
      case false => sorted.map(_.value)
      case true => get(false).map { case 14 => 1; case x => x }.sortWith(_ > _)
    }

    val a: List[Int] = get(false)
    val b: List[Int] = get(true)

    (a.max to a.min by -1).toList == a || (b.max to b.min by -1).toList == b
  }

  def sorted: List[CardCheck] = this.cards.sortWith(_.value > _.value) // used in isConsecutive

  override def toString: String = {
    cards.map(_.toString).mkString(" ")
  }

  private def group(n: Int): Map[String, List[CardCheck]] = {
    this.cards.groupBy(_.face).filter { case (a, b) => b.length == n }
  }

  private def isValid: Boolean = {
    cards.length == 5
  }
}


object HandCheck {
  def apply(cards: List[String]): HandCheck = {
    var ss = ""
    val c = cards.collect {
      case s: String =>
        ss = s
        Try {
          val pattern = "([0-9]+)([A-Za-z])".r
          val pattern(num, str) = ss
          if (num == "10") {
            ss = "T" + str
          }
        }
        new CardCheck(ss)
    }

    new HandCheck(c)
  }
}

object HH {

  def apply(cards: List[String]): HandCheck = {

    val c = cards.collect {
      case s: String =>

        new CardCheck(s)

    }
    new HandCheck(c)
  }
}







