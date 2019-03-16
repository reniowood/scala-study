package com.jinhyuk.scalawithcats.chapter4

import org.scalatest.{FlatSpec, Matchers}

class DbReaderSpec extends FlatSpec with Matchers {
  import Db._

  "DbReader" should "check login" in {
    val users = Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo"
    )

    val passwords = Map(
      "dade"  -> "zerocool",
      "kate"  -> "acidburn",
      "margo" -> "secret"
    )

    val db = Db(users, passwords)

    checkLogin(1, "zerocool").run(db) shouldBe true
    checkLogin(4, "davinci").run(db) shouldBe false
  }
}
