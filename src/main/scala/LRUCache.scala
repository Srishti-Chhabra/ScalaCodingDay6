import scala.collection.mutable.Map

case class Node(key: Int, value: Int, var prev: Node = null, var next: Node = null)

class LRUCache(_capacity: Int) {
  private val cacheMap = Map.empty[Int, Node]

  private val head: Node = Node(-1,-1)
  private val tail: Node = Node(-1,-1)
  head.next = tail
  tail.prev = head

  def get(key: Int): Int = {
    cacheMap.get(key).map{node =>
      remove(node)
      add(node)
      node.value
    }.getOrElse(-1)
  }

  def put(key: Int, value: Int): Unit = {
    if(cacheMap.contains(key)){
      remove(cacheMap(key))
      cacheMap.remove(key)
      val node: Node = Node(key,value)
      add(node)
      cacheMap.put(key,node)
    }
    else{
      if(cacheMap.size < _capacity){
        val node: Node = Node(key,value)
        add(node)
        cacheMap.put(key,node)
      }
      else{
        val nodeToBeDeleted: Node = remove(tail.prev)
        cacheMap.remove(nodeToBeDeleted.key)
        val node: Node = Node(key,value)
        add(node)
        cacheMap.put(key,node)
      }
    }
  }

  def add(node: Node): Unit = {
    node.next = head.next
    head.next.prev = node
    head.next = node
    node.prev = head
  }

  def remove(node: Node): Node = {
    node.prev.next = node.next
    node.next.prev = node.prev
    node
  }

}

/**
 * Your LRUCache object will be instantiated and called as such:
 * val obj = new LRUCache(capacity)
 * val param_1 = obj.get(key)
 * obj.put(key,value)
 */