package com.evolution.bootcamp.homework.adt

object AlgebraicDataTypes {


  // Homework. Define all algebraic data types, which would be needed to implement “Hold’em Hand Strength”
  // task you completed to join the bootcamp. Use your best judgement about particular data types to include
  // in the solution, you can model concepts like:
  //
  // 1. Suit
  // 2. Rank
  // 3. Card
  // 4. Hand (Texas or Omaha)
  // 5. Board
  // 6. Poker Combination (High Card, Pair, etc.)
  // 7. Test Case (Board & Hands to rank)
  // 8. Test Result (Hands ranked in a particular order for a particular Board, accounting for splits)
  //
  // Make sure the defined model protects against invalid data. Use value classes and smart constructors as
  // appropriate. Place the solution under `adt` package in your homework repository.

  // Attributions and useful links:
  // https://nrinaudo.github.io/scala-best-practices/definitions/adt.html
  // https://alvinalexander.com/scala/fp-book/algebraic-data-types-adts-in-scala/
  // https://en.wikipedia.org/wiki/Algebraic_data_type

  object Solver
  // Input string into adt's
  object Parser
  // Comparator for final hand states
  object Comparator

  val RANKS: Vector[String] = Vector("A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2")
  val SUITS: Vector[String] = Vector("h", "d", "c", "s")

  final case class Rank private (rank: String) extends AnyVal
  object Rank {
    def create(rank: String): Option[Rank] =
      if (RANKS.contains(rank)) Some(Rank(rank))
      else None
  }

  final case class Suit private (suit: String) extends AnyVal
  object Suit {
    def create(suit: String): Option[Suit] =
      if (SUITS.contains(suit)) Some(Suit(suit))
      else None
  }

  final case class Card(rank: Rank, suit: Suit)

  sealed trait Hand {
    def getBestCombination(board: Board): HandState
  }
  object Hand {
    sealed abstract case class HandTexas private (cards: List[Card]) extends Hand
    object HandTexas {
      def create(cards: List[Card]): Option[HandTexas] =
        if (cards.length == 2) Some(new HandTexas(cards) {
          override def getBestCombination(board: Board): HandState = ???
        })
        else None
    }
    sealed abstract case class HandOmaha private(cards: List[Card]) extends Hand
    object HandOmaha {
      def create(cards: List[Card]): Option[HandOmaha] =
        if (cards.length == 5) Some(new HandOmaha(cards) {
          override def getBestCombination(board: Board): HandState = ???
        })
        else None
    }
  }

  sealed abstract case class Board private (cards: List[Card])
  object Board {
    def create(cards: List[Card]): Option[Board] = {
      if (cards.length == 5) Some(new Board(cards) {})
      else None
    }
  }

  // The hand and its best combination
  final case class HandState(hand: Hand, combinationState: CombinationState)

  // Best combination for current hand
  final case class CombinationState(combination: Combination, combinationCards: List[Card], kicker: List[Card])

  //  abstract class Combination(weight: Int){
  //    def isCombination(board: Board, hand: Hand): Option[HandState]
  //  }

  sealed trait Combination {
    val weight: Int
    // Returns HandState if the given combination is correct for the hand, None if not correct
    def isCombination(board: Board, hand: Hand): Option[HandState]
  }
  object Combination {
    final case object StraightFlush extends Combination {
      override val weight: Int = ???
      override def isCombination(board: Board, hand: Hand): Option[HandState] = ???
    }
    final case object FourOfAKind extends Combination {
      override val weight: Int = ???
      override def isCombination(board: Board, hand: Hand): Option[HandState] = ???
    }
    final case object FullHouse extends Combination {
      override val weight: Int = ???
      override def isCombination(board: Board, hand: Hand): Option[HandState] = ???
    }
    final case object Flush extends Combination {
      override val weight: Int = ???
      override def isCombination(board: Board, hand: Hand): Option[HandState] = ???
    }
    final case object Straight extends Combination {
      override val weight: Int = ???
      override def isCombination(board: Board, hand: Hand): Option[HandState] = ???
    }
    final case object ThreeOfAKind extends Combination {
      override val weight: Int = ???
      override def isCombination(board: Board, hand: Hand): Option[HandState] = ???
    }
    final case object TwoPairs extends Combination {
      override val weight: Int = ???
      override def isCombination(board: Board, hand: Hand): Option[HandState] = ???
    }
    final case object Pair extends Combination {
      override val weight: Int = ???
      override def isCombination(board: Board, hand: Hand): Option[HandState] = ???
    }
    final case object HighCard extends Combination {
      override val weight: Int = ???
      override def isCombination(board: Board, hand: Hand): Option[HandState] = ???
    }
  }

}
