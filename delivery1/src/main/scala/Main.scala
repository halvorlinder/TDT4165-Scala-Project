@main def hello: Unit = 
  // println(array(50))
  // println(sum(array(100)))
  // println(rec_sum(array(100)))
  // println(fib(10))
  // val t = Concurrency.getThread(hello2)
  // t.start()
  // for (i <- 1 to 100000){
  //   val t1 = Concurrency.getThread(Concurrency.increaseCounterSynchronized)
  //   val t2 = Concurrency.getThread(Concurrency.increaseCounterSynchronized)
  //   val t3 = Concurrency.getThread(Concurrency.printCounter)
  //   t1.start()
  //   t2.start()
  //   t1.join()
  //   t2.join()
  //   t3.start()
  //   t3.join()
  //   Concurrency.resetCounter() 
  // }
  val t4 = Concurrency.getThread(()=>{ print(B.y) })
  val t5 = Concurrency.getThread(()=>{ print(A.x) })
  t4.start()
  t5.start()
  t4.join()
  t5.join()

def array(n : Int ) : Array[Int] = {
  (for (i <- 1 to n) yield i).toArray
}

def sum(list : Array[Int]) : Int = {
  var sum = 0
  var n = 0
  for (n <- list.toList){
    sum+=n
  }
  sum
}

def rec_sum(list: Array[Int]): Int = {
  def recSumInternal(list: Array[Int], acc : Int): Int = {
    list match {
      case Array() => acc
      case _ => recSumInternal(list.tail, acc + list.head)
    }
  }
  recSumInternal(list, 0)
}

def fib(n : Int) : BigInt = {
  n match {
    case 0 => BigInt(0)
    case 1 => BigInt(1)
    case _ => fib(n-1) + fib(n-2)
  }
}

def hello2 ()= {println("hello")}

object Concurrency
{ 
  private var counter: Int = 0

  def getThread (f : ()=>Unit) : Thread = { 
    new Thread(new Runnable {
      def run() = {
        f()
      }
    }) 
  }
  def increaseCounter() : Unit = {
    counter+=1
  }
  def increaseCounterSynchronized() : Unit = counter.synchronized{
    counter+=1
  }
  def printCounter() : Unit = {
    if (counter==1) print(1)
  }
  def resetCounter(): Unit = {
    counter = 0
  }
}
object A { lazy val x: Int = B.y }
object B { lazy val y: Int = A.x }
