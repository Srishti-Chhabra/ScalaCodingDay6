class RepeatingAndMissingNumberInAGrid {
  def findMissingAndRepeatedValues(grid: Array[Array[Int]]): Array[Int] = {
    val n: Int = grid.size
    val totalSum: Int =  ((n*n) * ((n*n)+1)) / 2
    val sum: Int = grid.map(row => row.sum).sum
    val hashArr: Array[Int] = Array.fill[Int](n*n + 1)(0)
    def findDuplicate(i: Int): Int = {
      if(i>=n)
        return -1
      def innerLoop(j: Int): Int = {
        if(j>=n)
          return -1
        if(hashArr(grid(i)(j))==0){
          hashArr(grid(i)(j)) += 1
          innerLoop(j+1)
        }
        else
          grid(i)(j)
      }
      val res = innerLoop(0)
      if(res == -1)
        findDuplicate(i+1)
      else
        res
    }
    val duplicate: Int = findDuplicate(0)
    Array(duplicate, totalSum - (sum - duplicate))
  }
}
