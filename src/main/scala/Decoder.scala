import Types.{Bit, Digit, Even, NoParity, Odd, One, Parity, Pixel, Str, Zero}

import java.util.Collections.swap
import scala.collection.immutable

object Decoder {
  // TODO 1.1
  def toBit(s: Char): Bit = {
    s match {
      case '0' => Zero
      case '1' => One
    }
  }
  def toBit(s: Int): Bit = {
    s match {
      case 0 => Zero
      case 1 => One
    }
  }

  // TODO 1.2
  def complement(c: Bit): Bit = {
    c match {
      case Zero => One
      case One => Zero
    }
  }

  // TODO 1.3
  val LStrings: List[String] = List("0001101", "0011001", "0010011", "0111101", "0100011",
    "0110001", "0101111", "0111011", "0110111", "0001011")

  val leftOddList: List[List[Bit]] = LStrings.map(str => str.map(toBit).toList) // codificări L

  val rightList: List[List[Bit]] = leftOddList.map(bits => bits.map(complement)) // codificări R

  val leftEvenList: List[List[Bit]] = rightList.map(bits => bits.reverse) // codificări G
  
  // TODO 1.4
  def group[A](l: List[A]): List[List[A]] = {
    l.foldRight(List[List[A]]()) { (currentElem, acc) =>
      acc match {
        case (currentGroup :: restOfGroups) if (currentGroup.head == currentElem) =>
          (currentElem :: currentGroup) :: restOfGroups
        case _ => List(currentElem) :: acc
      }
    }
  }
  
  // TODO 1.5
  def runLength[A](l: List[A]): List[(Int, A)] = {
    group(l).map(elem => (elem.length, elem.head))
  }
  
  case class RatioInt(n: Int, d: Int) extends Ordered[RatioInt] {
    require(d != 0, "Denominator cannot be zero")
    private val gcd = BigInt(n).gcd(BigInt(d)).toInt
    val a = n / gcd // numărător
    val b = d / gcd // numitor

    override def toString: String = s"$a/$b"

    override def equals(obj: Any): Boolean = obj match {
      case that: RatioInt => this.a.abs == that.a.abs &&
        this.b.abs == that.b.abs &&
        this.a.sign * this.b.sign == that.a.sign * that.b.sign
      case _ => false
    }

    // TODO 2.1
    def -(other: RatioInt): RatioInt = {
      val leftNumerator = n * other.d
      val rightNumerator = other.n * d
      val commonDenominator = d * other.d

      RatioInt(leftNumerator - rightNumerator, commonDenominator)
    }
    def +(other: RatioInt): RatioInt = {
      val leftNumerator = n * other.d
      val rightNumerator = other.n * d
      val commonDenominator = d * other.d

      RatioInt(leftNumerator + rightNumerator, commonDenominator)
    }
    def *(other: RatioInt): RatioInt = {
      RatioInt(n * other.n, d * other.d)
    }
    def /(other: RatioInt): RatioInt = {
      RatioInt(n * other.d, d * other.n)
    }

    // TODO 2.2
    def compare(other: RatioInt): Int = {
      val difference = (this - other).n
      if (difference < 0) {
        -1
      } else if (difference > 0) {
        1
      } else {
        0
      }
    }
  }
  
  // TODO 3.1
  def scaleToOne[A](l: List[(Int, A)]): List[(RatioInt, A)] = {
    val totalElements = l.foldLeft(0)((acc, currentPair) => acc + currentPair._1)
    l.map(currentPair => (RatioInt(currentPair._1, totalElements), currentPair._2))
  }

  // TODO 3.2
  def scaledRunLength(l: List[(Int, Bit)]): (Bit, List[RatioInt]) = {
    (l.head._2, scaleToOne(l).map(currentPair => currentPair._1))
  }
  
  // TODO 3.3
  def toParities(s: Str): List[Parity] = {
    def toParity(c: Char): Parity = {
      c match {
        case 'G' => Even
        case 'L' => Odd
      }
    }
    s.map(toParity)
  }
  
  // TODO 3.4
  val PStrings: List[String] = List("LLLLLL", "LLGLGG", "LLGGLG", "LLGGGL", "LGLLGG",
    "LGGLLG", "LGGGLL", "LGLGLG", "LGLGGL", "LGGLGL")
  val leftParityList: List[List[Parity]] = PStrings.map(str => toParities(str.toList))

  // TODO 3.5
  type SRL = (Bit, List[RatioInt])

  def toSRL(l: List[List[Bit]]): List[SRL] = {
    l.map(listOfBits => scaledRunLength(runLength(listOfBits)))
  }

  val leftOddSRL:  List[SRL] = toSRL(leftOddList)
  val leftEvenSRL:  List[SRL] = toSRL(leftEvenList)
  val rightSRL:  List[SRL] = toSRL(rightList)

  // TODO 4.1
  def distance(l1: SRL, l2: SRL): RatioInt = ???
  
  // TODO 4.2
  def bestMatch(SRL_Codes: List[SRL], digitCode: SRL): (RatioInt, Digit) = ???
  
  // TODO 4.3
  def bestLeft(digitCode: SRL): (Parity, Digit) = ???
  
  // TODO 4.4
  def bestRight(digitCode: SRL): (Parity, Digit) = ???

  def chunkWith[A](f: List[A] => (List[A], List[A]))(l: List[A]): List[List[A]] = {
    l match {
      case Nil => Nil
      case _ =>
        val (h, t) = f(l)
        h :: chunkWith(f)(t)
    }
  }
  
  def chunksOf[A](n: Int)(l: List[A]): List[List[A]] =
    chunkWith((l: List[A]) => l.splitAt(n))(l)

  // TODO 4.5
  def findLast12Digits(rle:  List[(Int, Bit)]): List[(Parity, Digit)] = ???

  // TODO 4.6
  def firstDigit(l: List[(Parity, Digit)]): Option[Digit] = ???

  // TODO 4.7
  def checkDigit(l: List[Digit]): Digit = ???
  
  // TODO 4.8
  def verifyCode(code: List[(Parity, Digit)]): Option[String] = ???
  
  // TODO 4.9
  def solve(rle:  List[(Int, Bit)]): Option[String] = ???
  
  def checkRow(row: List[Pixel]): List[List[(Int, Bit)]] = {
    val rle = runLength(row);

    def condition(sl: List[(Int, Pixel)]): Boolean = {
      if (sl.isEmpty) false
      else if (sl.size < 59) false
      else sl.head._2 == 1 &&
        sl.head._1 == sl.drop(2).head._1 &&
        sl.drop(56).head._1 == sl.drop(58).head._1
    }

    rle.sliding(59, 1)
      .filter(condition)
      .toList
      .map(_.map(pair => (pair._1, toBit(pair._2))))
  }
}


