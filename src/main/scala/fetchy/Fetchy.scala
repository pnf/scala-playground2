package fetchy

import fetch.Query
import cats.data.NonEmptyList
import cats.data.NonEmptyList
import cats.instances.list._
import fetch._

import scala.concurrent._
import scala.concurrent.duration._


object Fetchy {

  type UserId = Int
  case class User(id: UserId, username: String)

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

  def getPost(id: PostId): Fetch[Post] = Fetch(id)
  def getAuthor(p: Post): Fetch[User] = Fetch(p.author)

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
  import cats.syntax.cartesian._ // for |@|

  // bummer that we don't have applicative for

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

  val homePage = (postsByAuthor |@| postTopics).tupled

  {
    import ExecutionContext.Implicits.global

    val res = Await.result(Fetch.run[Future](homePage), Duration.Inf)

    println(res)
  }

  {
    import monix.eval.Task
    import monix.execution.Scheduler
    import fetch.monixTask.implicits._
    val scheduler = Scheduler.Implicits.global

    val task = Fetch.run[Task](homePage)

    val res = Await.result(task.runAsync(scheduler), Duration.Inf)
    println(res)
  }

}
