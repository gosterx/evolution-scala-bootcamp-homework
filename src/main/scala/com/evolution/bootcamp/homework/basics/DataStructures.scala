package com.evolution.bootcamp.homework.basics

import scala.annotation.tailrec

object DataStructures extends App {
  // https://leetcode.com/problems/running-sum-of-1d-array/
  def runningSum(nums: Array[Int]): Array[Int] = {
    nums.scanLeft(0)(_ + _).tail
  }


  // https://leetcode.com/problems/shuffle-the-array
  def shuffle(nums: Array[Int], n: Int): Array[Int] = {
    nums.take(n).zip(nums.drop(n)).flatMap { case (x, y) => Array(x, y) }
  }

  // https://leetcode.com/problems/richest-customer-wealth
  def maximumWealth(accounts: Array[Array[Int]]): Int = {
    accounts.map(_.sum).max
  }

  // https://leetcode.com/problems/kids-with-the-greatest-number-of-candies/
  def kidsWithCandies(candies: Array[Int], extraCandies: Int): Array[Boolean] = {
    candies.map(x => if ((x + extraCandies) >= candies.max) true else false)
  }

  // https://leetcode.com/problems/widest-vertical-area-between-two-points-containing-no-points
  def maxWidthOfVerticalArea(points: Array[Array[Int]]): Int = {
    val sortedByXArray = points.map(_.head).sorted

    def helper(array: Array[Int], acc: Int): Int = {
      array.toList match {
        case _ :: Nil | Nil => acc
        case x :: y :: _ => if (y - x > acc) helper(array.tail, y - x) else helper(array.tail, acc)
      }
    }

    helper(sortedByXArray, -1)
  }

  // https://leetcode.com/problems/maximum-nesting-depth-of-the-parentheses/
  def maxDepth(s: String): Int = {

    @tailrec
    def helper(list: List[Char], currentDepth: Int, maxDepth: Int): Int = {
      list match {
        case Nil => maxDepth
        case x :: _ if x == '(' =>
          if (currentDepth + 1 > maxDepth) helper(list.tail, currentDepth + 1, currentDepth + 1)
          else helper(list.tail, currentDepth + 1, maxDepth)
        case x :: _ if x == ')' => helper(list.tail, currentDepth - 1, maxDepth)
        case _ => helper(list.tail, currentDepth, maxDepth)
      }
    }

    helper(s.toList, 0, 0)
  }

  // https://leetcode.com/problems/split-a-string-in-balanced-strings
  def balancedStringSplit(s: String): Int = {

    @tailrec
    def helper(list: List[Char], currentValue: Int, maxAmountOfBalancedString: Int): Int = {
      list match {
        case Nil => maxAmountOfBalancedString
        case x :: _ if x == 'R' =>
          if (currentValue + 1 == 0) helper(list.tail, currentValue + 1, maxAmountOfBalancedString + 1)
          else helper(list.tail, currentValue + 1, maxAmountOfBalancedString)
        case x :: _ if x == 'L' =>
          if (currentValue - 1 == 0) helper(list.tail, currentValue - 1, maxAmountOfBalancedString + 1)
          else helper(list.tail, currentValue - 1, maxAmountOfBalancedString)
        case _ => helper(list.tail, currentValue, maxAmountOfBalancedString)
      }
    }

    helper(s.toList, 0, 0)
  }


  @tailrec
  def allVegetablesEqual[T](list: List[T], f: (T, T) => Boolean): Boolean = {
    list match {
      case Nil => false
      case _ :: Nil => true
      case x :: y :: _ =>
        if (f(x, y)) allVegetablesEqual(list.tail, f)
        else false
    }
  }

  val vegetableAmounts = Map(
    "pumpkins" -> 234,
    "olives" -> 32,
    "cucumbers" -> 323,
  )

  val vegetableWeights = Map(
    ("pumpkins", 10),
    ("cucumbers", 20),
    ("olives", 2),
  )

  val totalVegetableCost: Int = {
    vegetableAmounts.values.sum
  }

  def allSubsetsOfSizeN[A](set: Set[A], n: Int): Set[Set[A]] = n match {
    case 1 => set.map(x => Set(x))
    case n if n > 1 =>
      for {
        x <- set
        subsets <- allSubsetsOfSizeN(set - x, n - 1)
      } yield subsets + x
    case _ => Set()
  }


  // Homework
  //
  // Implement a special sort which sorts the keys of a map (K) according to their associated
  // values (V).
  //
  // In case of "ties" (equal values) it should group these keys K into Set-s in the results.
  //
  // The input is a map where keys (K) are the values to be sorted and values are their associated numeric
  // values.
  //
  // The output is a list (in ascending order according to the associated `Int` values) of tuples of `Set`-s
  // with values from K, and the associated value V for these values in the `Set`.
  //
  // For example:
  //
  // Input `Map("a" -> 1, "b" -> 2, "c" -> 4, "d" -> 1, "e" -> 0, "f" -> 2, "g" -> 2)` should result in
  // output `List(Set("e") -> 0, Set("a", "d") -> 1, Set("b", "f", "g") -> 2, Set("c") -> 4)`.
  def sortConsideringEqualValues[T](map: Map[T, Int]): List[(Set[T], Int)] = {
    val groupedMap = map.toList.groupBy{ case(_, y) => y}.values.toList
    val numbers = groupedMap.map(_.head).map{ case (_, y) => y }
    val letters = groupedMap.map(element => element.map{ case (x, _) => x }).map(_.toSet)
    letters zip numbers
  }
}
