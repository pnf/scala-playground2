package fetchy

import java.util.concurrent.ScheduledThreadPoolExecutor

import applicator.LiftFetchTuples
import cats.data.NonEmptyList
import cats.effect.ConcurrentEffect
import cats.instances.list._
import cats.effect._
import cats.temp.par._
//import cats.instances.list._
import cats.syntax.all._
import fetch._
import cats.syntax.all._
import cats.syntax.apply._
import scala.concurrent.ExecutionContext

import fetch.syntax._


object Fetchy {


  val blah = 2
  // https://www.scala-exercises.org/fetch/concurrency_monads

  type AuthorId = Int

  case class Author(id: AuthorId, Authorname: String)

  type Quality = Int
  type Popularity = Int

  def latency[F[_] : ConcurrentEffect : Par, A](result: A, msg: String) = {
    val id = Thread.currentThread.getId
    Sync[F].delay(println(s"Requesting [tid=$id] $msg")) >>
      Sync[F].delay(Thread.sleep(100)) >>
      Sync[F].delay(println(s"Receiving [tid=$id] $msg")) >>
      Sync[F].pure(result)
  }



  implicit object AuthorSource extends DataSource[AuthorId, Author] {
    private val AuthorDatabase: Map[AuthorId, Author] = Map(
      1 -> Author(1, "@one"),
      2 -> Author(2, "@two"),
      3 -> Author(3, "@three"),
      4 -> Author(4, "@four")
    )

    override def name = "Author"

    override def fetch[F[_] : ConcurrentEffect : Par](id: AuthorId) =
      latency(AuthorDatabase.get(id), "One Author id")

    override def batch[F[_] : ConcurrentEffect : Par](ids: NonEmptyList[Int]) =
      latency(AuthorDatabase.filterKeys(ids.toList.contains), s"${ids.size} Authors")
  }
  def getAuthor[F[_]: ConcurrentEffect : Par](id: AuthorId): Fetch[F, Author] = Fetch(id, AuthorSource)

  /*
  def fetch[F[_] : ConcurrentEffect : Par](id: I): F[Option[A]]
  def batch[F[_] : ConcurrentEffect : Par](ids: NonEmptyList[I]): F[Map[I, A]] =
*/

  type PostId = Int

  case class Post(id: PostId, author: AuthorId, content: String)

  implicit object PostSource extends DataSource[PostId, Post] {
    override def name = "Post"

    private val postDatabase: Map[PostId, Post] = Map(
      1 -> Post(1, 2, "An article"),
      2 -> Post(2, 3, "Another article"),
      3 -> Post(3, 4, "Yet another article")
    )

    override def fetch[F[_] : ConcurrentEffect : Par](id: PostId) =
      latency(postDatabase.get(id), "One post request")

    override def batch[F[_] : ConcurrentEffect : Par](ids: NonEmptyList[PostId]) =
      latency(postDatabase.filterKeys(ids.toList.contains), s"${ids.size} post requests")
  }
  def getPost[F[_] : ConcurrentEffect: Par](id: PostId): Fetch[F, Post] = Fetch(id, PostSource)

  implicit object QualitySource extends DataSource[String, Quality] {
    override def name = "Quality"

    override def fetch[F[_] : ConcurrentEffect : Par](text: String): F[Option[Quality]] =
      latency(Some(text.length), "One quality request")

    override def batch[F[_] : ConcurrentEffect : Par](texts: NonEmptyList[String]): F[Map[String, Quality]] =
      latency(texts.map(s ⇒ (s, s.length)).toList.toMap, s"${texts.size} qualities")
  }
  def getQuality[F[_] : ConcurrentEffect: Par](content: String): Fetch[F, Quality] = Fetch(content, QualitySource)

  implicit object PopularitySource extends DataSource[Post, Popularity] {
    override def name = "Popularity"

    override def fetch[F[_] : ConcurrentEffect : Par](p: Post): F[Option[Popularity]] =
      latency(Some(p.id % 3), "One popularity request")

    override def batch[F[_] : ConcurrentEffect : Par](ps: NonEmptyList[Post]): F[Map[Post, Popularity]] =
      latency(ps.map(p ⇒ (p, p.id % 3)).toList.toMap, s"${ps.size} popularity requests")
  }
  def getPopularity[F[_] : ConcurrentEffect: Par](p: Post): Fetch[F, Popularity] = Fetch(p, PopularitySource)

  type PostTopic = String

  implicit object PostTopicSource extends DataSource[Post, PostTopic] {
    override def name = "Post topic"

    private def topic(p: Post) = if (p.id % 2 == 0) "monad" else "applicative"

    override def fetch[F[_] : ConcurrentEffect : Par](p: Post) =
      latency(Some(topic(p)), "One topic request")

    override def batch[F[_] : ConcurrentEffect : Par](ps: NonEmptyList[Post]) =
      latency(ps.map(p ⇒ (p, topic(p))).toList.toMap, "Many topic requests")
  }
  def getTopic[F[_] : ConcurrentEffect: Par](p: Post): Fetch[F, PostTopic] = Fetch(p, PostTopicSource)







}


object TestBFF extends App {
  import cats._
  import fetch._
  import cats.Id
  import fetch.syntax._
  import cats.instances.list._
  import cats.syntax.traverse._
  import cats.syntax.apply._
  import cats.syntax.semigroupal._
  import cats._
  import fetch._



  type PersonId = Int
  type LastName = String
  type FirstName = String
  type BFF = PersonId
  trait DictSource[A,B] extends DataSource[A,B]  {
    protected val dict: Map[A,B]
    private def deliverWithLatency[F[_] : ConcurrentEffect : Par, A](result: A, msg: String) = {
      val id = Thread.currentThread.getId
      Sync[F].delay(println(s"Requesting [tid=$id] $msg")) >>
        Sync[F].delay(Thread.sleep(100)) >>
        Sync[F].delay(println(s"Receiving [tid=$id] $msg")) >>
        Sync[F].pure(result)
    }
    override def fetch[F[_] : ConcurrentEffect : Par](a: A) =
      deliverWithLatency(dict.get(a), s"One $name")
    override def batch[F[_] : ConcurrentEffect : Par](as: NonEmptyList[A]) =
      deliverWithLatency(dict.filterKeys(as.toList.contains), s"${as.size} x $name")
  }
  implicit object BFFSource extends DictSource[PersonId, BFF] {
    override val name = "BFF"
    override val dict = Map(1 → 2, 2 → 3, 3 → 1)
  }
  def getBFF[F[_]: ConcurrentEffect : Par](id: PersonId) = Fetch(id, BFFSource)
  implicit object FirstNameSource extends DictSource[PersonId, FirstName] {
    override val name = "First Name"
    override val dict = Map(1 → "Biff", 2 → "Heinz", 3 → "Tree")
  }
  def getFirstName[F[_]: ConcurrentEffect : Par](id: PersonId) = Fetch(id, FirstNameSource)
  implicit object LastNameSource extends DictSource[PersonId, LastName] {
    override val name = "Last Name"
    override val dict = Map(1 → "Loman", 2 → "Doofenshmirtz", 3 → "Trunks")
  }
  def getLastName[F[_]: ConcurrentEffect : Par](id: PersonId) = Fetch(id, LastNameSource)

  def getBFF1[F[_]: ConcurrentEffect : Par](id: PersonId): Fetch[F, String] = for {
    bff ← getBFF(id)
    fn ← getFirstName(bff)
    ln ← getLastName(bff)
  } yield s"$fn $ln"
  def getBFFs1[F[_]: ConcurrentEffect : Par] = List(1, 2, 3).traverse(getBFF1[F])

  def getBFFs[F[_]: ConcurrentEffect : Par] = List(1, 2, 3).traverse { id ⇒
    for {
      bff ← getBFF(id)
      fn ← getFirstName(bff)
      ln ← getLastName(bff)
    } yield s"$fn $ln"
  }

  // Help out intellij!
  implicit def myTuple2Semigroupal[F[_]: ConcurrentEffect, A, B](t2: (Fetch[F,A], Fetch[F,B])) = cats.implicits.catsSyntaxTuple2Semigroupal(t2)


  def getBFF2[F[_]: ConcurrentEffect : Par](id: PersonId): Fetch[F, String] = for {
    bff ← getBFF(id)
    (fn,ln) ← (getFirstName(bff), getLastName(bff)).tupled
  } yield s"$fn $ln"
  def getBFFs2[F[_]: ConcurrentEffect : Par]: Fetch[F,List[String]] = List(1, 2, 3).traverse(getBFF2[F])

  def getBFF3[F[_]: ConcurrentEffect : Par](id: PersonId): Fetch[F, String] = LiftFetchTuples {
    for {
      bff ← getBFF(id)
      fn ← getFirstName(bff)
      ln ← getLastName(bff)
    } yield s"$fn $ln"
  }
  def getBFFs3[F[_]: ConcurrentEffect : Par] = List(1,2,3).traverse(getBFF3[F])

  val executor = new ScheduledThreadPoolExecutor(4)
  val executionContext: ExecutionContext = ExecutionContext.fromExecutor(executor)

  implicit val timer: Timer[IO] = IO.timer(executionContext)
  implicit val cs: ContextShift[IO] = IO.contextShift(executionContext)

  println("First try:\n" + Fetch.run[IO](getBFFs1).unsafeRunSync())
  println("Manual applicative:\n" + Fetch.run[IO](getBFFs2).unsafeRunSync())
  println("Auto applicative:\n" + Fetch.run[IO](getBFFs3).unsafeRunSync())

}



object Test extends App {

  import Fetchy._
  import cats.Id
  import fetch.syntax._
  import cats.instances.list._
  import cats.syntax.traverse._
  import cats.syntax.apply._
  import cats.syntax.semigroupal._
  import cats._
  import fetch._


  def authorByPostId[F[_] : ConcurrentEffect : Par](id: PostId): Fetch[F, Author] = for {
    post: Post ← getPost(id)
    author: Author ← getAuthor(post.author)
  } yield author

  def postsByAuthor[F[_] : ConcurrentEffect : Par : ContextShift]: Fetch[F, List[Post]] = for {
    posts: List[Post] <- List(1, 2).traverse(getPost(_))
    authors: List[Author] <- posts.traverse(p ⇒ getAuthor(p.author))
    ordered = (posts zip authors).sortBy({ case (_, author) => author.Authorname }).map(_._1)
  } yield ordered

  def postTopics[F[_] : ConcurrentEffect : Par]: Fetch[F, Map[PostTopic, Int]] = for {
    posts: List[Post] <- List(2, 3).traverse(getPost(_))
    topics: List[PostTopic] <- posts.traverse(getTopic(_))
    countByTopic = (posts zip topics).groupBy(_._2).mapValues(_.size)
  } yield countByTopic

  def homePage[F[_] : ConcurrentEffect : Par : ContextShift] = (postsByAuthor, postTopics).tupled // (postsByAuthor |@| postTopics).tupled


  def getAP[F[_] : ConcurrentEffect : Par](id: PostId): Fetch[F, (Post, Author, Quality)] = for {
    p: Post ← getPost(id)
    a: Author ← getAuthor(p.author)
    q: Quality ← getQuality(p.content)
  } yield (p, a, q)

  def getAPx[F[_] : ConcurrentEffect: Par] = List(1,2,3).traverse(getAP(_))

  def getAPy[F[_] : ConcurrentEffect: Par] = List(1,2,3).traverse { id ⇒
    for {
      p: Post ← getPost(id)
      a: Author ← getAuthor(p.author)
      q: Quality ← getQuality(p.content)
    } yield (p, a, q)
  }


  def getAP2[F[_] : ConcurrentEffect: Par](id: PostId): Fetch[F, (Post, Author, Quality)] = {
    LiftFetchTuples {
      for {
        p: Post ← getPost(id)
        a: Author ← getAuthor(p.id)
        q: Quality ← getQuality(p.content)
      } yield (p, a, q)
    }
  }


  def getAP2x[F[_] : ConcurrentEffect: Par] = List(1,2,3).traverse(getAP2(_))


  import scala.concurrent.duration._

  {
    import java.util.concurrent._

    val executor = new ScheduledThreadPoolExecutor(4)
    val executionContext: ExecutionContext = ExecutionContext.fromExecutor(executor)

    implicit val timer: Timer[IO] = IO.timer(executionContext)
    implicit val cs: ContextShift[IO] = IO.contextShift(executionContext)

    println("homepage\n" + Fetch.run[IO](homePage).unsafeRunTimed(5.seconds))
    println("APx\n" + Fetch.run[IO](getAPx).unsafeRunTimed(5.seconds))
    println("AP2x\n" + Fetch.run[IO](getAP2x).unsafeRunTimed(5.seconds))


  }
}
