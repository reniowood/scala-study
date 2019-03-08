package com.jinhyuk.scalawithcats.chapter1

final case class Cat(name: String, age: Int, color: String)

object Cat1 {
  import PrintableInstances._

  implicit val catPrintable = new Printable[Cat] {
    override def format(cat: Cat): String = {
      val name = Printable.format(cat.name)
      val age = Printable.format(cat.age)
      val color = Printable.format(cat.color)

      s"$name is a $age year-old $color cat."
    }
  }

  def main(args: Array[String]): Unit = {
    val cat = Cat("보리", 30, "blue")

    Printable.print(cat)

    import PrintableSyntax._

    cat.print
  }
}

object Cat2 {
  import cats._
  import cats.instances.int._
  import cats.instances.string._
  import cats.syntax.show._

  implicit val catShow: Show[Cat] = Show.show { cat =>
    val name = cat.name.show
    val age = cat.age.show
    val color = cat.color.show

    s"$name is a $age year-old $color cat."
  }

  def main(args: Array[String]): Unit = {
    val cat = Cat("보리", 30, "blue")

    println(cat.show)
  }

}

object Cat3 {
  import cats._
  import cats.instances.int._
  import cats.instances.string._
  import cats.syntax.eq._

  implicit val catEq: Eq[Cat] =
    Eq.instance[Cat] { (cat1, cat2) =>
      cat1.name === cat2.name && cat1.age === cat2.age && cat1.color === cat2.color
    }

  def main(args: Array[String]): Unit = {
    val cat1 = Cat("Garfield", 38, "orange and black")
    val cat2 = Cat("Heathcliff", 33, "orange and black")

    println(cat1 === cat2)
    println(cat1 =!= cat2)

    import cats.instances.option._

    val optionCat1 = Option(cat1)
    val optionCat2 = Option.empty[Cat]

    println(optionCat1 === optionCat2)
    println(optionCat1 =!= optionCat2)
  }

}
