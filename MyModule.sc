
def factorial(n: Int): Int = {
  @annotation.tailrec
  def go(n: Int, acc: Int): Int = {
    if(n <= 0) acc
    else go(n-1, acc * n)
  }

  go(n,1)
}
//factorial(5)

def fibonacci(m: Int): Int = {
  var tmp1, tmp2 = 0
  def go(m: Int, f: Int, s: Int): Int = {
    if(m <= 0) s
    else {
      go(m-1,s,f+s)
    }
  }
  go(m,0,1)
}
//fibonacci(4)

//Higher Order Function
def formatResult(name: String, n: Int, f: Int => Int) = {
  val msg = "The %s of %d is %d."
  msg.format(name, n, f(n))
}
//formatResult("fibonacci number", 5, fibonacci)

def findFirst[T](arr: Array[T], f: T => Boolean): Int = {
  @annotation.tailrec
  def loop(n: Int): Int = {
    if(n >= arr.length) -1
    else if(f(arr(n))) n
    else loop(n+1)
  }
  loop(0)
}

//findFirst( Array("a","b","c","d"), (x: String) => x == "c" )

def isSorted[A](arr: Array[A], f: (A,A) => Boolean): Boolean = {
  var bl = false
  @annotation.tailrec
  def loop(n: Int): Boolean = {
    if(n >= arr.length - 1) bl
    else if( f(arr(n), arr(n+1)) ){
      bl = true
      loop(n+1)
    }
    else{
      false
    }
  }
  loop(0)
}
// asc
isSorted(Array(1,2,3,4,5,7,8,10), (x: Int, y: Int) => x < y)
// desc
isSorted(Array(1,2,3,4,5,7,8,10).reverse, (x: Int, y: Int) => x > y)