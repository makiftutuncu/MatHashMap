import scala.collection.mutable

object Benchmark extends App {
  val matHashMap: MatHashMap[String, Int] = new MatHashMap[String, Int]
  val hashMap: mutable.HashMap[String, Int] = new mutable.HashMap[String, Int]

  val count = 100000
  val key = "key_"

  val time1 = System.currentTimeMillis()
  for(i <- 1 to count) matHashMap.put(key + i, i)
  println(s"Added $count items to MatHashMap in ${System.currentTimeMillis() - time1} ms.\n")

  val time2 = System.currentTimeMillis()
  for(i <- 1 to count) hashMap.put(key + i, i)
  println(s"Added $count items to HashMap in ${System.currentTimeMillis() - time2} ms.\n")

  val time3 = System.currentTimeMillis()
  for(i <- 1 to count) matHashMap.get(key + i)
  println(s"Accessed $count items from MatHashMap in ${System.currentTimeMillis() - time3} ms.\n")

  val time4 = System.currentTimeMillis()
  for(i <- 1 to count) hashMap.get(key + i)
  println(s"Accessed $count items from HashMap in ${System.currentTimeMillis() - time4} ms.\n")

  val time5 = System.currentTimeMillis()
  for(i <- 1 to count) matHashMap.delete(key + i)
  println(s"Deleted $count items from MatHashMap in ${System.currentTimeMillis() - time5} ms.\n")

  val time6 = System.currentTimeMillis()
  for(i <- 1 to count) hashMap.remove(key + i)
  println(s"Deleted $count items from HashMap in ${System.currentTimeMillis() - time6} ms.\n")
}
