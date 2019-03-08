package com.jinhyuk.scalawithcats.chapter1

trait Printable[A] {
  def format(value: A): String
}

object PrintableInstances {
  implicit val intPrintable = new Printable[Int] {
    override def format(input: Int): String = input.toString
  }

  implicit val stringPrintable = new Printable[String] {
    override def format(input: String): String = input
  }
}

object Printable {
  def format[A](input: A)(implicit p: Printable[A]): String = p.format(input)
  def print[A](input: A)(implicit p: Printable[A]): Unit = println(p.format(input))
}

object PrintableSyntax {
  implicit class PrintableOps[A](input: A) {
    def format(implicit p: Printable[A]): String = p.format(input)
    def print(implicit p: Printable[A]): Unit = println(p.format(input))
  }
}