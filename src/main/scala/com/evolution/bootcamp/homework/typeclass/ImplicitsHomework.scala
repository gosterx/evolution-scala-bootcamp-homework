package com.evolution.bootcamp.homework.typeclass

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

object ImplicitsHomework {

  /**
   * Lo and behold! Brand new super-useful collection library for Scala!
   *
   * Our main guest today - [[SuperVipCollections4s.MutableBoundedCache]],
   * a specially crafted, mutable but non-thread-safe (sic!), key-value in-memory cache which bounds the size
   * of the data stored.
   *
   * As the real memory footprint of values on JVM is clouded in mystery, for data size estimation we use
   * a thing called size score. Its calculation rules:
   * - size score of a Byte is 1
   * - Int - 4 (as primitive JVM int consists of 4 bytes)
   * - Long - 8
   * - Char - 2 (one UTF-16 symbol is 2 bytes)
   * - String - 12 (supposedly the size of the JVM object header) + length * size score of Char
   * - score for any case class is 12 (again our folk wisdom about JVM object layout) + sum of scores of all
   * the fields
   * - score for any sequence (Array[T], List[T], Vector[T]) is
   * 12 (our old friend object header) + sum of scores of all elements
   * - score for any Map[K, V] is 12 + sum of scores of all keys + sum of scores of all values
   */

  object SuperVipCollections4s {
    type SizeScore = Int
    type Error = String
    val ByteSize: SizeScore = 1
    val IntSize: SizeScore = 4
    val LongSize: SizeScore = 8
    val CharSize: SizeScore = 2
    val ObjectHeaderSize: SizeScore = 12


    trait GetSizeScore[T] {
      def apply(value: T): SizeScore
    }

    object syntax {

      implicit class GetSizeScoreOps[T: GetSizeScore](inner: T) {
        def sizeScore: SizeScore = {
          val getSizeScore = implicitly[GetSizeScore[T]]
          getSizeScore(inner)
        }
      }

    }

    /**
     * Mutable key-value cache which limits the size score of the data scored.
     *
     * The size score of the data is sum of size scores of all keys + sum of size scores of all values.
     * If upon insertion the total score gets over [[maxSizeScore]], the oldest KV-pairs
     * (in the insertion order) should be evicted. If even with the eviction of all the existing elements,
     * the KV-pair can't be added without violating [[maxSizeScore]] - the behaviour is undefined.
     *
     * @param maxSizeScore max size score for the stored data
     * @tparam K key type
     * @tparam V value type
     */
    final class MutableBoundedCache[K: GetSizeScore, V: GetSizeScore](maxSizeScore: SizeScore) {
      //with this you can use .sizeScore syntax on keys and values

      import instances._
      import syntax._

      /*
      mutable.LinkedHashMap is a mutable map container which preserves insertion order - this might be useful!
       */
      private val map = mutable.LinkedHashMap.empty[K, V]

      def currentMapSize: SizeScore = {
        map.sizeScore - map.empty.sizeScore
      }

      def removeFirstElement(): Either[Error, Unit] = {
        val headKey = map.head match {
          case (k, _) => k
        }
        map.remove(headKey).toRight("Can't remove element from empty map").map(_ => ())
      }

      def put(key: K, value: V): Either[Error, Unit] = {
        val totalPairScore = key.sizeScore + value.sizeScore

        if (totalPairScore <= maxSizeScore) {
          if (currentMapSize + totalPairScore > maxSizeScore) {
            removeFirstElement().flatMap(_ => put(key, value))
          } else {
            map.put(key, value).toRight("Can't put element").map(_ => ())
          }
        } else {
          Left("This pair is too large for this cache")
        }
      }

      def get(key: K): Option[V] = map.get(key)

    }

    /**
     * Cool custom immutable multi-map collection - does not extend the standard library collection types
     * (yes, this is a feature)
     */
    final case class PackedMultiMap[K, +V](inner: ArraySeq[(K, V)])

    object PackedMultiMap {
      def empty[K, V]: PackedMultiMap[K, V] = PackedMultiMap()

      def apply[K, V](values: (K, V)*): PackedMultiMap[K, V] = PackedMultiMap(inner = ArraySeq(values: _*))
    }

    /**
     * Type-class allowing us to iterate over different "collection-like" types with one type arg
     */
    trait Iterate[-F[_]] {
      def iterator[T](f: F[T]): Iterator[T]
    }

    object Iterate {
      def apply[F[_] : Iterate]: Iterate[F] = implicitly[Iterate[F]]
    }

    /**
     * Same as [[Iterate]] but for collections containing 2 types of values (think Map's and like)
     */
    trait Iterate2[-F[_, _]] {
      def iterator1[T, S](f: F[T, S]): Iterator[T]

      def iterator2[T, S](f: F[T, S]): Iterator[S]
    }

    object Iterate2 {
      def apply[F[_, _] : Iterate2]: Iterate2[F] = implicitly[Iterate2[F]]
    }

    object instances {

      import syntax._

      implicit val iterableOnceIterate: Iterate[Iterable] = new Iterate[Iterable] {
        override def iterator[T](f: Iterable[T]): Iterator[T] = f.iterator
      }
      //Array is not an Iterable in Scala 2.13 but we still might abstract over iteration logic for both!
      implicit val arrayIterate: Iterate[Array] = new Iterate[Array] {
        override def iterator[T](f: Array[T]): Iterator[T] = f.iterator
      }
      implicit val mutableMapIterate: Iterate2[mutable.LinkedHashMap] = new Iterate2[mutable.LinkedHashMap] {
        override def iterator1[T, S](f: mutable.LinkedHashMap[T, S]): Iterator[T] = f.keys.iterator

        override def iterator2[T, S](f: mutable.LinkedHashMap[T, S]): Iterator[S] = f.values.iterator
      }
      implicit val mapIterate: Iterate2[Map] = new Iterate2[Map] {
        override def iterator1[T, S](f: Map[T, S]): Iterator[T] = f.keys.iterator

        override def iterator2[T, S](f: Map[T, S]): Iterator[S] = f.values.iterator
      }
      implicit val packedMapIterate: Iterate2[PackedMultiMap] = new Iterate2[PackedMultiMap] {
        override def iterator1[T, S](f: PackedMultiMap[T, S]): Iterator[T] = f.inner.map { case (k, _) => k }.iterator

        override def iterator2[T, S](f: PackedMultiMap[T, S]): Iterator[S] = f.inner.map { case (_, v) => v }.iterator
      }
      //Provide Iterate2 instances for Map and PackedMultiMap!
      //if the code doesn't compile while you think it should - sometimes full rebuild helps!
      /*
      replace this big guy with proper implicit instances for types:
      - Byte, Char, Int, Long
      - String
      - Array[T], List[T], Vector[T], Map[K,V], PackedMultiMap[K,V]
        - points to karma if you provide those in a generic way
        (Iterate and Iterate2 type-classes might be helpful!)
      If you struggle with writing generic instances for Iterate and Iterate2, start by writing instances for
      List and other collections and then replace those with generic instances.
       */

      implicit def SizeScoreIterable[F[_] : Iterate, T: GetSizeScore]: GetSizeScore[F[T]] = new GetSizeScore[F[T]] {
        override def apply(value: F[T]): SizeScore = ObjectHeaderSize + Iterate[F].iterator(value).map(_.sizeScore).sum
      }

      implicit def SizeScoreMap[F[_, _] : Iterate2, T: GetSizeScore, S: GetSizeScore]: GetSizeScore[F[T, S]] = new GetSizeScore[F[T, S]] {
        override def apply(value: F[T, S]): SizeScore = ObjectHeaderSize + Iterate2[F].iterator1(value).map(_.sizeScore).sum + Iterate2[F].iterator2(value).map(_.sizeScore).sum
      }

      implicit val SizeScoreByte: GetSizeScore[Byte] = new GetSizeScore[Byte] {
        override def apply(value: Byte): SizeScore = ByteSize
      }
      implicit val SizeScoreInt: GetSizeScore[Int] = new GetSizeScore[Int] {
        override def apply(value: Int): SizeScore = IntSize
      }
      implicit val SizeScoreChar: GetSizeScore[Char] = new GetSizeScore[Char] {
        override def apply(value: Char): SizeScore = CharSize
      }
      implicit val SizeScoreLong: GetSizeScore[Long] = new GetSizeScore[Long] {
        override def apply(value: Long): SizeScore = LongSize
      }
      implicit val SizeScoreString: GetSizeScore[String] = new GetSizeScore[String] {
        override def apply(value: String): SizeScore = ObjectHeaderSize + value.length * CharSize
      }
    }

  }

  /*
  Time to bring some business value!
  #GoodVibes #ThrowbackThursday #NoFilter #squadgoals
   */
  object MyTwitter {

    import SuperVipCollections4s._
    import instances._
    import syntax._

    final case class Twit(
                           id: Long,
                           userId: Int,
                           hashTags: Vector[String],
                           attributes: PackedMultiMap[String, String],
                           fbiNotes: List[FbiNote],
                         )

    final case class FbiNote(
                              month: String,
                              favouriteChar: Char,
                              watchedPewDiePieTimes: Long,
                            )

    trait TwitCache {
      def put(twit: Twit): Either[Error, Unit]

      def get(id: Long): Option[Twit]
    }

    implicit val TwitSizeScore: GetSizeScore[Twit] = new GetSizeScore[Twit] {
      override def apply(value: Twit): SizeScore = value match {
        case Twit(id, userId, hashTags, attributes, fbiNotes) =>
          ObjectHeaderSize + id.sizeScore + userId.sizeScore + hashTags.sizeScore + attributes.sizeScore + fbiNotes.sizeScore
      }
    }

    implicit val fbiNoteSizeScore: GetSizeScore[FbiNote] = new GetSizeScore[FbiNote] {
      override def apply(value: FbiNote): SizeScore = value match {
        case FbiNote(month, favouriteChar, watchedPewDiePieTimes) =>
          ObjectHeaderSize + month.sizeScore + favouriteChar.sizeScore + watchedPewDiePieTimes.sizeScore
      }
    }

    /*
    Return an implementation based on MutableBoundedCache[Long, Twit]
     */
    def createTwitCache(maxSizeScore: SizeScore): TwitCache = new TwitCache {

      val boundedCache = new MutableBoundedCache[Long, Twit](maxSizeScore)

      override def put(twit: Twit): Either[Error, Unit] = boundedCache.put(twit.id, twit)

      override def get(id: Long): Option[Twit] = boundedCache.get(id)
    }
  }

}
