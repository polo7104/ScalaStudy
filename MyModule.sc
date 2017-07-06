
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




