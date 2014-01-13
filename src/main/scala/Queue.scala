//implement the following api with the indicated complexities
//This should be an *immutable* queue
// 
// could also use a finger tree
trait Queue[T] {
  
  val enqueueList: List[T]
  val dequeueList: List[T]

  //O(1)
  def isEmpty: Boolean = enqueueList.isEmpty && dequeueList.isEmpty
  //O(1)
  def insert(t: T): Queue[T] = enqueue(t)

  def enqueue(t: T): Queue[T] = 
    Queue( t :: enqueueList, dequeueList )

  //O(1) -- this isn't
  def head: Option[T] = enqueueList.headOption

  //O(1) amortised
  def dequeue: (T, Queue[T]) = 
   if (dequeueList.isEmpty) { 
      val dq = enqueueList.reverse
      (dq.head, Queue(List(), dq.tail))
    } else { 
      (dequeueList.head, Queue(enqueueList, dequeueList.tail))
    }
 
  //O(1) amortised
  // need dequeulist to be 
  def tail: Queue[T] = 
    if (enqueueList.isEmpty)
      Queue( dequeueList.reverse.tail, List())
    else    
      Queue( enqueueList.tail, dequeueList)
}
 
object Queue {
  def apply[T](enqList: List[T], deqList: List[T] = List()) =
    new Queue[T] { 
      val enqueueList = enqList
      val dequeueList = deqList
    }

  def empty[T]: Queue[T] = Queue(List())
}
