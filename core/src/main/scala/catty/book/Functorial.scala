package catty.book

object Functorial {

  trait Printable[A] {
    def format(value: A): String
    def contramap[B](f: B => A) = {val dis=this; new Printable[B] { def format(value: B) = dis.format(f(value)) }
    }
  }
  object Printable {
    def format[A](a: A)(implicit inst: Printable[A]) = inst.format(a)
  }



  implicit val booleanPrintable = new Printable[Boolean] {def format(v: Boolean) = if(v) "y" else "n" }
  Printable.format(true)

  final case class Box[A](v: A)
  implicit def boxPrintable[A: Printable] = new Printable[Box[A]] {
    override def format(value: Box[A]): String = s"[${Printable.format(value.v)}]"
  }

  Printable.format(Box(true))



}
