package com.poker

object HandTypeList {
  def all: List[HandType] = {
    List(new StraightFlush,
      new FourOfAKind,
      new FullHouse,
      new Flush,
      new Straight,
      new ThreeOfAKind,
      new TwoPair,
      new Pair,
      new HighCard
    )
  }
}

trait HandType {
  def evaluate(h: HandCheck): (Boolean, HandType, List[Int])
}

class FourOfAKind extends HandType {
  def evaluate(h: HandCheck): (Boolean, HandType, List[Int]) = {
    def score: List[Int] = {
      val p: List[Int] = h.quads.collect { case (a, b) => b.head.value }.toList
      val q: List[Int] = h.sorted.map(_.value).filter(!p.contains(_))

      List(7) ::: p ::: q ::: List(0, 0, 0)
    }

    (h.quads.toList.length == 1, this, score)
  }

  override def toString: String = {
    "Four of a Kind"
  }
}

object FourOfAKind {
  def apply(h: HandCheck): (Boolean, HandType, List[Int]) = (new FourOfAKind).evaluate(h)
}


class Flush extends HandType {
  def evaluate(h: HandCheck): (Boolean, HandType, List[Int]) = {
    (h.isSameSuit, this, 5 :: h.sorted.map(_.value))
  }

  override def toString: String = {
    "Flush"
  }
}

object Flush {
  def apply(h: HandCheck): (Boolean, HandType, List[Int]) = (new Flush).evaluate(h)
}


class FullHouse extends HandType {
  def evaluate(h: HandCheck): (Boolean, HandType, List[Int]) = {

    def matches: Boolean = {
      h.pairs.toList.length == 1 && h.trips.toList.length == 1
    }

    def score: List[Int] = {
      val p: List[Int] = matches match {
        case true => h.trips.collect { case (a, b) => b.head.value }.toList
        case false => 0 :: Nil
      }

      val q: List[Int] = matches match {
        case true => h.pairs.collect { case (a, b) => b.head.value }.toList
        case false => 0 :: Nil
      }

      6 :: p.head :: q.head :: 0 :: 0 :: 0 :: Nil
    }

    (matches, this, score)
  }

  override def toString: String = {
    "Full House"
  }
}

object FullHouse {
  def apply(h: HandCheck): (Boolean, HandType, List[Int]) = (new FullHouse).evaluate(h)
}


class Pair extends HandType {
  def evaluate(h: HandCheck): (Boolean, HandType, List[Int]) = {
    def score: List[Int] = {
      val p: List[Int] = h.pairs.collect { case (a, b) => b.head.value }.toList
      val q: List[Int] = h.sorted.map(_.value).filter(!p.contains(_))

      List(1) ::: p ::: q ::: List(0)
    }

    //println(h.pairs.toList.length == 1, this, score)
    (h.pairs.toList.length == 1, this, score)
  }

  override def toString: String = {
    "Pair"
  }
}

object Pair {
  def apply(h: HandCheck): (Boolean, HandType, List[Int]) = (new Pair).evaluate(h)
}


class Straight extends HandType {
  def evaluate(h: HandCheck): (Boolean, HandType, List[Int]) = {
    (h.isConsecutive, this, 4 :: h.sorted.map(_.value))
  }

  override def toString: String = {
    "Straight"
  }
}


object Straight {
  def apply(h: HandCheck): (Boolean, HandType, List[Int]) = (new Straight).evaluate(h)
}


class StraightFlush extends HandType {
  def evaluate(h: HandCheck): (Boolean, HandType, List[Int]) = {

    def matches: Boolean = {
      h.isConsecutive && h.isSameSuit
    }

    (matches, this, 8 :: h.sorted.map(_.value))
  }

  override def toString: String = {
    "Straight Flush"
  }

}

object StraightFlush {
  def apply(h: HandCheck): (Boolean, HandType, List[Int]) = (new StraightFlush).evaluate(h)
}

class ThreeOfAKind extends HandType {
  def evaluate(h: HandCheck): (Boolean, HandType, List[Int]) = {
    def score: List[Int] = {
      val p: List[Int] = h.trips.collect { case (a, b) => b.head.value }.toList
      val q: List[Int] = h.sorted.map(_.value).filter(!p.contains(_))

      List(3) ::: p ::: q ::: List(0, 0)
    }

    (h.trips.toList.length == 1, this, score)
  }

  override def toString: String = {
    "Three of a Kind"
  }
}

object ThreeOfAKind {
  def apply(h: HandCheck): (Boolean, HandType, List[Int]) = (new ThreeOfAKind).evaluate(h)
}


class TwoPair extends HandType {


  def evaluate(h: HandCheck): (Boolean, HandType, List[Int]) = {
    def score: List[Int] = {
      val p: List[Int] = h.pairs.collect { case (a, b) => b.head.value }.toList
      val q: List[Int] = h.sorted.map(_.value).filter(!p.contains(_))

      List(2) ::: p ::: q ::: List(0, 0)
    }

    // println(h.pairs.toList.length == 1, this, score+     "Two pair")
    (h.pairs.toList.length == 2, this, score)
  }

  override def toString: String = {
    "Two Pair"
  }
}

object TwoPair {
  def apply(h: HandCheck): (Boolean, HandType, List[Int]) = (new TwoPair).evaluate(h)
}

class HighCard extends HandType {
  def evaluate(h: HandCheck): (Boolean, HandType, List[Int]) = {
    (true, this, 0 :: h.sorted.map(_.value))
  }

  override def toString: String = "High Card"
}

object HighCard {
  def apply(h: HandCheck): (Boolean, HandType, List[Int]) = (new HighCard).evaluate(h)
}






