package com.jinhyuk.essentialscala.typeclasses

sealed trait JsValue {
  def stringify: String
}

final case class JsObject(values: Map[String, JsValue]) extends JsValue {
  def stringify: String = values
    .map { case (name, value) => "\"" + name + "\":" + value.stringify }
    .mkString("{", ",", "}")
}

final case class JsString(value: String) extends JsValue {
  def stringify: String = "\"" + value.replaceAll("\\|\"", "\\\\$1") + "\""
}

trait JsWriter[A] {
  def write(value: A): JsValue
}

object JsUtil {
  def toJson[A](value: A)(implicit writer: JsWriter[A]): JsValue = writer write value
}

import java.util.Date

sealed trait Visitor {
  def id: String
  def createdAt: Date
  def age: Long = new Date().getTime() - createdAt.getTime()
}

final case class Anonymous(
                            id: String,
                            createdAt: Date = new Date()
                          ) extends Visitor

final case class User(
                       id: String,
                       email: String,
                       createdAt: Date = new Date()
                     ) extends Visitor

object JsWriterInstances {
  implicit object AnonymousWriter extends JsWriter[Anonymous] {
    override def write(anonymous: Anonymous): JsValue =
      JsObject(
        Map(
          "id" -> JsString(anonymous.id),
          "createdAt" -> JsString(anonymous.createdAt.toString)
        )
      )
  }

  implicit object UserWriter extends JsWriter[User] {
    override def write(user: User): JsValue =
      JsObject(
        Map(
          "id" -> JsString(user.id),
          "email" -> JsString(user.email),
          "createdAt" -> JsString(user.createdAt.toString)
        )
      )
  }

  implicit object VisitorWriter extends JsWriter[Visitor] {
    def write(value: Visitor): JsValue = value match {
      case anon: Anonymous => JsUtil.toJson(anon)
      case user: User      => JsUtil.toJson(user)
    }
  }
}

object JsonSerialization {
  implicit class JsonOps[A](value: A) {
    def toJson(implicit writer: JsWriter[A]): JsValue = writer.write(value)
  }

  def main(args: Array[String]): Unit = {
    import JsWriterInstances._

    val visitors: Seq[Visitor] = Seq(Anonymous("001", new Date), User("003", "dave@xample.com", new Date))

    visitors.foreach(visitor => println(JsUtil.toJson(visitor)))

    visitors.foreach(visitor => println(visitor.toJson))
  }
}