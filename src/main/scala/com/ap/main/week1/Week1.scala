package com.ap.main.week1

object Week1 {

  def abs(d: Double): Double = if (d > 0) d else -d

  //SqaureRoot
  def sq(d: Double): Double = {

    def isGoodEnough(guess: Double): Boolean =
      abs(guess * guess - d) / d < 0.001

    def improve(guess: Double): Double =
      (guess + d / guess) / 2

    def sqrtIter(guess: Double): Double = {
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))
    }

    sqrtIter(1.0)
  }



}
