package com.ap.main.week2

import scala.annotation.tailrec

object Week2 {

  def factorial(n: Int) : Int = {
    @tailrec
    def factIter(acc: Int, n: Int) : Int = {
      if(n==0) acc
      else factIter(acc * n, n-1)
    }
    factIter(1, n)
  }

  //Sum of a function (cube, fact or itself) in a range a to b
  def sumInitial(f: Int => Int, a : Int, b: Int) :  Int = {
    if(a>b) 0 else f(a) + sumInitial(f, a+1, b)
  }

}

object exercise{
  val tolerance = 0.0001

  def abs(x: Double):Double = if(x>0) x else -x

  def isGoodEnough(guess: Double, x: Double): Boolean= {
    abs((guess-x)/x)/x < tolerance
  }

  def getFixedPoint(f: Double => Double) (firstGuess: Double) : Double ={
    def iterate(guess: Double): Double = {
      val fGuess = f(guess)
      if(isGoodEnough(guess, fGuess)) fGuess
      else iterate(fGuess)
    }
    getFixedPoint(f) (firstGuess)
   }
}
