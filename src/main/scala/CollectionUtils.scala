package poc

import scala.collection.immutable

/**
  * Created by pavel on 14/07/2017.
  */
object CollectionUtils {
  /**
    * Count values in sequence.
    *
    * @param values values
    * @tparam T type
    */
  implicit class CountValues[T](values: Seq[T]) {
    def countValues[N](implicit t: Numeric[N]): Map[T, N] = {
      values.groupBy(identity).mapValues(x => t.fromInt(x.length)).toSerializable
    }
  }

  /**
    * Implicit conversion map to serializable map (it's bug in scala).
    *
    * @param map map
    * @tparam K key type
    * @tparam V value type
    */
  implicit class SerializableMap[K, V](map: scala.collection.Map[K, V]) {
    def toSerializable: immutable.Map[K, V] = {
      map.toMap.map(identity)
    }
  }

}
