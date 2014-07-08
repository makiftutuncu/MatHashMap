/**
 * A custom generic mutable hash map implementation
 *
 * @tparam K  Type of the keys
 * @tparam V  Type of the values
 */
class MatHashMap[K, V] {
  // Size of the internal array to hold the items
  private val BUCKET_SIZE: Int = 8192

  // Internal array to hold the items, initialized as empty items
  private val buffer: Array[MatHashMapNode[K, V]] = Array.fill(BUCKET_SIZE) { MatEmptyItem }

  /** Number of items in the hash map */
  var count = 0

  /**
   * Puts a new item to the hash map
   *
   * @param key   Key of the item
   * @param value Value of the item
   *
   * @return      true if successfully put, false if any error occurred
   */
  def +(key: K, value: V): Boolean = put(key, value)

  /**
   * Puts a new item to the hash map
   *
   * @param key   Key of the item
   * @param value Value of the item
   *
   * @return      true if successfully put, false if any error occurred
   */
  def put(key: K, value: V): Boolean = {
    val bucketIndex: Int = getBucketIndex(key)
    val item: MatHashMapNode[K, V] = buffer(bucketIndex)

    item match {
      case MatEmptyItem =>
        buffer(bucketIndex) = MatItem[K, V](key, value, MatEmptyItem)
        count += 1
        true
      case MatItem(_, _, _) =>
        val putToNodeResult: Boolean = putToNode(key, value, item.asInstanceOf[MatItem[K, V]])
        if(putToNodeResult) count += 1
        putToNodeResult
    }
  }

  /**
   * Gets the value of item with given key from the hash map
   *
   * @param key Key of the item
   *
   * @return    Value of item with given key optionally
   */
  def get(key: K): Option[V] = {
    val bucketIndex: Int = getBucketIndex(key)
    val item: MatHashMapNode[K, V] = buffer(bucketIndex)

    item match {
      case MatEmptyItem =>
        None
      case MatItem(_, _, _) =>
        getFromNode(key, item.asInstanceOf[MatItem[K, V]])
    }
  }

  /**
   * Deletes the item with given key from the hash map
   *
   * @param key Key of the item
   *
   * @return    true if successfully deleted, false if any error occurred
   */
  def -(key: K): Boolean = delete(key)

  /**
   * Deletes the item with given key from the hash map
   *
   * @param key Key of the item
   *
   * @return    true if successfully deleted, false if any error occurred
   */
  def delete(key: K): Boolean = {
    val bucketIndex: Int = getBucketIndex(key)
    val item: MatHashMapNode[K, V] = buffer(bucketIndex)

    item match {
      case MatEmptyItem =>
        false
      case MatItem(_, _, _) =>
        deleteFromNode(key, item.asInstanceOf[MatItem[K, V]], item.asInstanceOf[MatItem[K, V]])
    }
  }

  /**
   * Checks whether hash map is empty or not
   *
   * @return  true if hash map is empty, false otherwise
   */
  def isEmpty: Boolean = count == 0

  /**
   * Checks whether hash map contains an item with given key or not
   *
   * @param key Key of the item
   *
   * @return    true if hash map contains an item with given key, false otherwise
   */
  def contains(key: K): Boolean = get(key).isDefined

  /**
   * Gets the bucket index corresponding to the given key
   *
   * @param key Key for which to find a bucket index
   *
   * @return    Bucket index corresponding to the given key
   */
  private def getBucketIndex(key: K): Int = math.abs(key.hashCode) % BUCKET_SIZE

  /**
   * Puts a new item to the hash map recursively
   *
   * @param key   Key of the item
   * @param value Value of the item
   * @param node  Current node to process
   *
   * @return      true if successfully put, false if any error occurred
   */
  private def putToNode(key: K, value: V, node: MatItem[K, V]): Boolean = {
    if(node.key == key) false else node.next match {
      case MatEmptyItem =>
        node.next = MatItem[K, V](key, value, MatEmptyItem)
        true
      case i @ MatItem(_, _, MatEmptyItem) if key != i.key =>
        i.next = MatItem[K, V](key, value, MatEmptyItem)
        true
      case i @ MatItem(_, _, MatItem(_, _, _)) if key != i.key =>
        putToNode(key, value, node.next.asInstanceOf[MatItem[K, V]])
      case _ =>
        println("Unknown put error!")
        false
    }
  }

  /**
   * Gets the value of item with given key from the hash map recursively
   *
   * @param key   Key of the item
   * @param node  Current node to process
   *
   * @return      Value of item with given key optionally
   */
  private def getFromNode(key: K, node: MatItem[K, V]): Option[V] = {
    if(node.key == key) Some(node.value) else node.next match {
      case MatEmptyItem =>
        None
      case MatItem(k: K, v: V, MatEmptyItem) if key == k =>
        Some(v)
      case MatItem(k: K, v: V, MatItem(_, _, _)) if key == k =>
        Some(v)
      case MatItem(_, _, MatEmptyItem) =>
        None
      case MatItem(_, _, MatItem(_, _, _)) =>
        getFromNode(key, node.next.asInstanceOf[MatItem[K, V]])
      case _ =>
        println("Unknown get error!")
        None
    }
  }

  /**
   * Deletes the item with given key from the hash map recursively
   *
   * @param key           Key of the item
   * @param previousNode  Previous node to process
   * @param node          Current node to process
   *
   * @return              true if successfully deleted, false if any error occurred
   */
  private def deleteFromNode(key: K, previousNode: MatItem[K, V], node: MatItem[K, V]): Boolean = {
    node match {
      case MatItem(k: K, v: V, MatEmptyItem) if key == k =>
        if(node == previousNode) {
          // This is the only node in the bucket and matches
          buffer(getBucketIndex(key)) = MatEmptyItem
          count -= 1
          true
        } else {
          // This is the last node in the bucket and matches
          previousNode.next = MatEmptyItem
          count -= 1
          true
        }
      case MatItem(k: K, v: V, MatEmptyItem) =>
        if(node == previousNode) {
          // This is the only node in the bucket but doesn't match
          false
        } else {
          // This is the last node in the bucket but doesn't match
          false
        }
      case MatItem(k: K, v: V, MatItem(_, _, _)) if key == k =>
        if(node == previousNode) {
          // This is the first node in the bucket and matches
          buffer(getBucketIndex(key)) = node.next
          count -= 1
          true
        } else {
          // This is a node in the middle of the bucket and matches
          previousNode.next = node.next
          count -= 1
          true
        }
      case MatItem(_, _, MatItem(_, _, _)) =>
        // This is either the first node or a node in the middle of the bucket but doesn't match
        deleteFromNode(key, node, node.next.asInstanceOf[MatItem[K, V]])
      case _ =>
        println("Delete error!")
        false
    }
  }
}

/**
 * A generic super type of nodes in the hash map
 *
 * @tparam K  Type of the keys
 * @tparam V  Type of the values
 */
sealed trait MatHashMapNode[+K, +V]

/**
 * An item to use in the hash map
 *
 * @param key   Key of the item
 * @param value Value of the item corresponding to the key
 * @param next  Next node if there is collision in the bucket of this item
 *
 * @tparam K    Type of the keys
 * @tparam V    Type of the values
 */
private case class MatItem[K, V](key: K, value: V, var next: MatHashMapNode[K, V]) extends MatHashMapNode[K, V]

/**
 * An empty item to use in the hash map
 */
private case object MatEmptyItem extends MatHashMapNode[Nothing, Nothing]