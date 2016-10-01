import scala.math

var factorial_cache = Array.ofDim[BigInt](1000)
var perms_cache = Array.ofDim[BigInt](1000)
var partitions_cache = Array.ofDim[List[List[Int]]](200,200)
var binomial_cache = Array.ofDim[BigInt](200, 200)
var countGraphs_cache = Array.ofDim[BigInt](200, 200)

def factorial(a: Int): BigInt = {
  if (factorial_cache(a) == null) {
    factorial_cache(a) = if (a == 0) 1
                         else factorial(a-1)*a
  }
  factorial_cache(a)
}

def binomial(a: Int, b: Int): BigInt = {
  if (binomial_cache(a)(b) == null) {
    binomial_cache(a)(b) = if (b > a) 0
       else if (a == b) 1
       else if (b == 0) 1
       else binomial(a - 1, b - 1) + binomial(a - 1, b)
  }
  binomial_cache(a)(b)
}

def partitions(n: Int, k: Int): List[List[Int]] = {
    if (partitions_cache(n)(k) == null) {
    partitions_cache(n)(k) = 
    if (k == 1) List(List(n))
    else if (n < k) List()
    else if (n == k) List(List.fill(k)(1))
    else {
      ( for { i <- 1 until n
          j <- partitions( n - i , k - 1 )
        } yield {
          i :: j
        } ).toList
      }
    }
    partitions_cache(n)(k)
}

def perms(n : List[Int]): BigInt = {
   if (n.length <= 1) 1 else {
     val num = ( n.tail.sum until n.sum ).map(x => BigInt(x + 1)).product
     val denoma = factorial(n.head)
     perms(n.tail)*num/denoma
   }
}

def countGraphs(V: Int, E: Int): BigDecimal = {
  ( for {
    i <- 1 until V + 1
  } yield {
    val res = ( for {
      j <- partitions(V, i) 
    } yield {
      val allGraphs = binomial(j.map(x => x*(x - 1)/2).sum, E)
      val normalizingConstant = if (i % 2 == 0) -perms(j) else perms(j)
      normalizingConstant*allGraphs
    } ).sum
    BigDecimal(res)/i
 } ).sum
}

countGraphs(20, 90)

