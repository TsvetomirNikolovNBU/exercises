

object Domashno3 {
 def main(args: Array[String]): Unit = {
  def from(n:Int): Stream[Int] = Stream.cons(n, from(n + 1))
  def multiplyByTwo(a: Int) : Stream[Int] = a #:: multiplyByTwo(2*a)
  lazy val power2 : Stream[Int] = multiplyByTwo(2)
  def fibFrom(fib1: Int, fib2: Int): Stream[Int] = fib1 #:: fibFrom(fib2,  fib1 + fib2)
  lazy val fib = fibFrom(1, 1)
  def maxFixedDigitsNumber(digit: Int) = math.pow(10, digit).toInt - 1
  def sumOfDigits(num: Int) = num.toString.map(_.asDigit).sum
  def sumOfDigitsPowThree(num: Int) =  num.toString.map(_.asDigit).map(math.pow(_, 3).toInt).sum
  lazy val pow3 = from(0).filter(a => a == sumOfDigitsPowThree(a));
  val capTask1 = from(1).take(10).toList.filter(x => math.pow(10, x) < x * math.pow(9,3) + 1).max + 1;
  lazy val rangeTask1 = (1 to capTask1 * math.pow(9, 3).toInt).toStream;
  lazy val resTask1 = rangeTask1.filter(a => a == sumOfDigitsPowThree(a))
  println("The fifth is " + fibFrom(1,1)(4))
  println("Task1 numbers are " + resTask1.toList)
  }
  }