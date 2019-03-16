package com.jinhyuk.scalawithcats.chapter4

import cats.data.Reader
import cats.syntax.applicative._

case class Db(usernames: Map[Int, String], passwords: Map[String, String])

object Db {
  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] = Reader(db => db.usernames.get(userId))
  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))
  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      username <- findUsername(userId)
      isOk <- username.map(username => checkPassword(username, password))
                      .getOrElse(false.pure[DbReader])
    } yield isOk
}
