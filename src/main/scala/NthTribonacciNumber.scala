class NthTribonacciNumber {
  def tribonacci(n: Int): Int = {
    def findTribonacci(n: Int, dp: Array[Int]): Int = {
      if(n == 0)
        return 0
      if(n == 1)
        return 1
      if(n == 2)
        return 1
      if(dp(n) != -1)
        return dp(n)
      dp(n) = findTribonacci(n-1,dp)+findTribonacci(n-2,dp)+findTribonacci(n-3,dp)
      return dp(n)
    }
    val dp: Array[Int] = Array.fill[Int](n+1)(-1)
    findTribonacci(n, dp)
  }
}
