class RepeatingAndMissingNumber {
  def findNumbers(nums: List[Int]): Array[Int] = {
    val n: Int = nums.size
    val totalSum: Int =  (n * (n+1)) / 2
    val sum: Int = nums.sum
    val hashArr: Array[Int] = Array.fill[Int](n+1)(0)
    def findDuplicate(i: Int): Int = {
      if(i>=n)
        return -1
      if(hashArr(nums(i))==0){
        hashArr(nums(i)) += 1
        findDuplicate(i+1)
      }
      else
        nums(i)
    }
    val duplicate: Int = findDuplicate(0)
    Array(duplicate, totalSum - (sum - duplicate))
  }
}

object RepeatingAndMissingNumber extends App{
  val repeatingAndMissingNumber = new RepeatingAndMissingNumber
  println(repeatingAndMissingNumber.findNumbers(List(1,3,3))(0))
}
