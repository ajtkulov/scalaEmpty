package main

object ReduceUtils {
  /**
    * Implicit class to add two maps
    *
    * @param fst values
    * @tparam K key types
    * @tparam V value types
    */
  implicit class MapPlus[K, V](fst: Map[K, V]) {
    def +(snd: Map[K, V])(implicit t: Numeric[V]): Map[K, V] = {
      val keys: Set[K] = fst.keySet ++ snd.keySet
      keys.map(key => {
        val fstValue: V = fst.getOrElse(key, t.zero)
        val sndValue: V = snd.getOrElse(key, t.zero)
        (key, t.plus(fstValue, sndValue))
      }).toMap
    }
  }
}
