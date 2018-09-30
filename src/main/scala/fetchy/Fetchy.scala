package fetchy

import applicator.TupleLiftable
import cats.Semigroupal
import fetch.Query
import cats.data.NonEmptyList
import cats.free.Free
import cats.instances.list._
import fetch._

import scala.concurrent._
import scala.concurrent.duration._
import applicator.TupleLiftable._

object FetchyImplicits {
  import cats.syntax.apply._
  implicit object FetchTupleLifter extends TupleLiftable[Fetch] {
    override def tupleLift[A, B](t: (Fetch[A], Fetch[B])): Fetch[(A, B)] = t.tupled
  }
}

object Fetchy {


  val blah = 11
  // https://www.scala-exercises.org/fetch/concurrency_monads

  type UserId = Int
  case class User(id: UserId, username: String)
  type Quality = Int
  type Popularity = Int

  def latency[A](result: A, msg: String) = {
    val id = Thread.currentThread.getId
    println(s"~~> [$id] $msg")
    Thread.sleep(100)
    println(s"<~~ [$id] $msg")
    result
  }

  val userDatabase: Map[UserId, User] = Map(
    1 -> User(1, "@one"),
    2 -> User(2, "@two"),
    3 -> User(3, "@three"),
    4 -> User(4, "@four")
  )

  implicit object UserSource extends DataSource[UserId, User]{
    override def name = "User"

    override def fetchOne(id: UserId): Query[Option[User]] = {
      Query.sync({
        latency(userDatabase.get(id), s"One User $id")
      })
    }
    override def fetchMany(ids: NonEmptyList[UserId]): Query[Map[UserId, User]] = {
      Query.sync({
        latency(userDatabase.filterKeys(ids.toList.contains), s"Many Users $ids")
      })
    }
  }
  def getUser(id: UserId): Fetch[User] = Fetch(id) // or, more explicitly: Fetch(id)(UserSource)

  type PostId = Int
  case class Post(id: PostId, author: UserId, content: String)


  val postDatabase: Map[PostId, Post] = Map(
    1 -> Post(1, 2, "An article"),
    2 -> Post(2, 3, "Another article"),
    3 -> Post(3, 4, "Yet another article")
  )

  implicit object PostSource extends DataSource[PostId, Post]{
    override def name = "Post"

    override def fetchOne(id: PostId): Query[Option[Post]] = {
      Query.sync({
        latency(postDatabase.get(id), s"One Post $id")
      })
    }
    override def fetchMany(ids: NonEmptyList[PostId]): Query[Map[PostId, Post]] = {
      Query.sync({
        latency(postDatabase.filterKeys(ids.toList.contains), s"Many Posts $ids")
      })
    }
  }

  implicit object QualitySource extends DataSource[String,Quality] {
    override def name = "Quality"
    override def fetchOne(id: String) = {
      Query.sync({
        latency(Some(id.length), "One quality")
      })
    }
    override def fetchMany(ids: NonEmptyList[String]): Query[Map[String, Int]] = {
      Query.sync({
        latency(ids.map(s ⇒ (s,s.length)).toList.toMap, "Many qualities")
      })
    }
  }

  implicit object PopularitySource extends DataSource[Post,Popularity] {
    override def name = "Popularity"
    override def fetchOne(p: Post) = {
      Query.sync({
        latency(Some(p.id*10), "One popularity")
      })
    }
    override def fetchMany(ids: NonEmptyList[Post]): Query[Map[Post, Int]] = {
      Query.sync({
        latency(ids.map(p ⇒ (p,p.id*10)).toList.toMap, "Many popularities")
      })
    }
  }


  def getPost(id: PostId): Fetch[Post] = Fetch(id)
  def getAuthor(p: Post): Fetch[User] = Fetch(p.author)
  def getQuality(s: String): Fetch[Quality] = Fetch(s)
  def getPopularity(p: Post): Fetch[Quality] = Fetch(p)

  type PostTopic = String

  implicit object PostTopicSource extends DataSource[Post, PostTopic]{
    override def name = "Post topic"

    override def fetchOne(id: Post): Query[Option[PostTopic]] = {
      Query.sync({
        val topic = if (id.id % 2 == 0) "monad" else "applicative"
        latency(Option(topic), s"One Post Topic $id")
      })
    }
    override def fetchMany(ids: NonEmptyList[Post]): Query[Map[Post, PostTopic]] = {
      Query.sync({
        val result = ids.toList.map(id => (id, if (id.id % 2 == 0) "monad" else "applicative")).toMap
        latency(result, s"Many Post Topics $ids")
      })
    }
  }

  def getPostTopic(post: Post): Fetch[PostTopic] = Fetch(post)


}


object Test extends App {
  import Fetchy._
  import cats.Id
  import fetch.unsafe.implicits._
  import fetch.implicits._
  import fetch.syntax._
  import cats.instances.list._
  import cats.syntax.traverse._
  import cats.syntax.apply._
  import cats.syntax.semigroupal._
  import cats._


  def authorByPostId(id: Int) = for {
    post ← getPost(id)
    author ← getAuthor(post)
  }  yield author

  val postsByAuthor: Fetch[List[Post]] = for {
    posts <- List(1, 2).traverse(getPost)
    authors <- posts.traverse(getAuthor)
    ordered = (posts zip authors).sortBy({ case (_, author) => author.username }).map(_._1)
  } yield ordered

  val postTopics: Fetch[Map[PostTopic, Int]] = for {
    posts <- List(2, 3).traverse(getPost)
    topics <- posts.traverse(getPostTopic)
    countByTopic = (posts zip topics).groupBy(_._2).mapValues(_.size)
  } yield countByTopic

  val homePage = (postsByAuthor, postTopics).tupled // (postsByAuthor |@| postTopics).tupled

  {
    import ExecutionContext.Implicits.global

    val res = Await.result(Fetch.run[Future](homePage), Duration.Inf)

    println(res)
  }

  {
    println("monix")

    import monix.eval.Task
    import monix.execution.Scheduler
    import fetch.monixTask.implicits._

    val scheduler = Scheduler.Implicits.global

    {
      val task = Fetch.run[Task](homePage)
      val res = Await.result(task.runAsync(scheduler), Duration.Inf)
      println(res)
    }
    {


      class bogusWithFilter[A](self: Fetch[A]) {
        def withFilter(p: A ⇒ Boolean) = new WithFilter(p)
        class WithFilter(p: A => Boolean) {
          def map[B](f: A => B): Fetch[B] = self.map {a ⇒
            if(!p(a)) throw new IllegalStateException
            else f(a)
          }
          def flatMap[B](f: A => Fetch[B]): Fetch[B] = self.flatMap {a ⇒
            if(!p(a)) throw new IllegalStateException
            else f(a)
          }
          def foreach[U](f: A => U): Unit = ???
          def withFilter(q: A => Boolean): WithFilter = new WithFilter(x => p(x) && q(x))
        }
      }

      def getAP(id: Int) = for {
        p ← getPost(id)
        (a,q) ← (getUser(p.author),getQuality(p.content)).tupled
        q ← getQuality(p.content)
      } yield (p,a,q)

      def getAP2(id: Int)=
        Fetchy.getPost(id)
          .flatMap((p: Post) =>
            catsSyntaxSemigroupal(Fetchy.getUser(p.author))(fetch.fetchApplicative)
              .product(Fetchy.getQuality(p.content))
              .flatMap({
                case (a, q) ⇒ Fetchy.getQuality(p.content).map((q: Int) => Tuple3.apply(p, a, q))
          }))



      import FetchyImplicits._

      def getAP3(id: Int): Free[FetchOp, (Post, User, Quality, Quality)] = {
        liftTuples {
        println("et voila")
          for {
            p ← getPost(id)
            a ← getUser(p.author)
            q ← getQuality(p.content)
            p2 ← getPopularity(p)
          } yield (p, a, q, p2)
        }
      }

        /*
    ~~> [12] Many Posts NonEmptyList(1, 2, 3)
    <~~ [12] Many Posts NonEmptyList(1, 2, 3)
    ~~> [12] Many popularities
    ~~> [13] Many qualities
    ~~> [15] Many Users NonEmptyList(2, 3, 4)
    <~~ [12] Many popularities
    <~~ [13] Many qualities
    <~~ [15] Many Users NonEmptyList(2, 3, 4)
           */




      /*
      def print[A](free: Fetch[A]): String = free match {
        case Free.Pure(p)     => s"Pure($p)"
        case Free.Suspend(fa) => s"Suspend($fa)"
        case Free.FlatMapped(free2, _) =>
          s"""FlatMapped(
             |  ${print(free2)},
             |  <fb => Free>)""".stripMargin
      }

*/


      def runny[T](f: Fetch[T]) = {
        val task = Fetch.run[Task](f)
        val res = Await.result(task.runAsync(scheduler), Duration.Inf)
        println(res)
      }

      val l = List(1,2,3)
      runny(l.traverse(getAP))
      runny(l.traverse(getAP2))
      runny(l.traverse(getAP3))

    }

  }

}



