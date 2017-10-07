package com.enjoysoft.scala

import java.nio.charset.Charset
import java.time.temporal.ChronoUnit
import java.time.{LocalDate, Period}


final case class Food(name: String, meat: Boolean, add: Boolean) {
  override def toString: String = name
}

object Food {
  private val Meat = '*'
  private val Add = '+'

  def apply(s: String): Food = Food(name(s), meat(s), add(s))

  private def meat(s: String) = s.head == Meat

  private def add(s: String) = s.last == Add

  private def name(s: String) = {
    val foo = if (meat(s)) s.drop(1) else s
    if (add(s)) foo.dropRight(1) else foo
  }
}

object Main extends App {

  val lines = scala.io.Source.fromFile("C:\\Users\\enjoy\\Desktop\\food.txt").getLines()

  val foods = (for (line <- lines) yield Food(line.trim())).toList

  var (newFoods, oldFoods) = foods.partition(_.add)

  var meats = oldFoods.filter(_.meat)
  var vegetables1 = oldFoods.filterNot(_.meat)
  var vegetables2 = rotate(vegetables1)

  val Days = 200
  val Week = 7
  val Birthday = LocalDate.of(2017, 2, 18)

  def rotate(list: List[Food]): List[Food] = {
    val head :: tail = list
    if (head == tail.head) tail else tail :+ head
  }

  val outputs = for (i <- 1 to Days) yield {

    val date = LocalDate.now().plusDays(i)
    val period = Period.between(Birthday, date)
    val months = period.getMonths
    val days = period.getDays
    val d = ChronoUnit.DAYS.between(Birthday, date)

    if (i % 3 == 1 && newFoods.nonEmpty) {
      // add new food every three days
      val newFood = newFoods.head
      newFoods = newFoods.tail
      if (newFood.meat) meats = newFood :: newFood :: newFood :: meats
      else {
        vegetables1 = newFood :: newFood :: newFood :: vegetables1
        vegetables2 = vegetables2.init :+ newFood :+ vegetables2.last
      }
    }

    val veg1 = vegetables1.head
    vegetables1 = rotate(vegetables1)
    val veg2 = vegetables2.head
    vegetables2 = rotate(vegetables2)
    val meat = meats.head
    meats = rotate(meats)

    val foo = s"$date,${months}月${days}天,$d,${veg1.name},${veg2.name},${meat.name}"
    println(foo)
    foo
  }

  import java.nio.file.{Files, Paths}

  import scala.collection.JavaConverters._

  Files.write(Paths.get("C:\\Users\\enjoy\\Desktop\\food.csv"), ("日期,月龄,天数,辅食1,辅食2,辅食3" +: outputs).asJava,
    Charset.forName("GB18030"))
}
