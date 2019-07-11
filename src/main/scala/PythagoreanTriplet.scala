import scala.collection.immutable

object PythagoreanTriplet {
  type Triple = (Int, Int, Int)
  
  def pythagoreanTriplets(min: Int, max: Int): Seq[Triple] = {
    
    val range: immutable.Seq[Int] = min to max
    
    val combos = for {
      s1 <- range
      s2 <- s1 + 1 to max
      s3 <- s2 + 1 to max
    } yield List(s1, s2, s3)
    
    combos.map(_.sorted).distinct.collect {
      case List(s1, s2, s3) if isPythagorean(Triple(s1, s2, s3)) => Triple(s1, s2, s3)
    }
  }

  def isPythagorean(triple: Triple): Boolean = triple match {
    case (a, o, h) if a * a + o * o == h * h => true
    case (a, h, o) if a * a + o * o == h * h => true
    case (h, o, a) if a * a + o * o == h * h => true
    case _ => false  
  }
} 