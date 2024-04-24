class FindTheNumberOccurringOddNumberOfTimes {
  def findNumber(nums: List[Int]): Int = {
    val n: Int = nums.size
    def loop(i: Int): Int = {
      if(i>=n)
        return -1
      def innerLoop(j: Int, count: Int): Int = {
        if(j>=n-1 || nums(j)!=nums(j+1))
          return count
        if(nums(j)==nums(j+1))
          innerLoop(j+1,count+1)
        else
          count
      }
      val count: Int = innerLoop(i,1)
      if(count % 2 == 1)
        nums(i)
      else
        loop(i+count)
    }
    loop(0)
  }
}

object FindTheNumberOccurringOddNumberOfTimes extends App{
  val nums: List[Int] = List(1, 2, 3, 2, 3, 1, 3)
  val findTheNumberOccurringOddNumberOfTimes = new FindTheNumberOccurringOddNumberOfTimes
  println(findTheNumberOccurringOddNumberOfTimes.findNumber(nums.sorted))
}
