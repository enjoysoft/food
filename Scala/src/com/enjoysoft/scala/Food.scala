package com.enjoysoft.scala

import java.nio.charset.Charset
import java.time.temporal.ChronoUnit
import java.time.{LocalDate, Period}


case class Food(name: String, meat: Boolean, add: Boolean)

object Food {
  val Meat = '*'
  val Add = '+'

  def apply(s: String): Food = Food(name(s), meat(s), add(s))

  private def meat(s: String) = s.head == Meat

  private def add(s: String) = s.last == Add

  private def name(s: String) = {
    val foo = if (meat(s)) s.drop(1) else s
    if (add(s)) foo.dropRight(1) else foo
  }
}

object Main extends App {

  val testTxtSource = scala.io.Source.fromFile("C:\\Users\\enjoy\\Desktop\\food.txt")

  val lines = testTxtSource.getLines()

  val foods: List[Food] = (for (line <- lines) yield Food(line.trim())).toList
  val Days = 100
  val Week = 6
  val Birthday = LocalDate.of(2017, 2, 16)
  var (newFoods, oldFoods) = foods.partition(_.add)
  var food1: Food = oldFoods.filterNot(_.meat).head
  var food2: Food = pickVeg(oldFoods, food1)
  var food3: Option[Food] = None
  var meatFoods: List[Food] = oldFoods.filter(_.meat)
  var meatQueue = Iterator.continually(meatFoods).flatten

  def pickVeg(foods: List[Food], foodEx: Food): Food = {
    val veg = foods.filterNot(_.meat)
    pickFood(veg, foodEx)
  }

  def pickFood(foods: List[Food], foodEx: Food): Food = scala.util.Random.shuffle(foods.filterNot(_ == foodEx)).head

  val outputs = for (i <- 1 to Days) yield {
    val date = LocalDate.now().plusDays(i)
    val period = Period.between(Birthday, date)
    val days = period.getDays
    val d = ChronoUnit.DAYS.between(Birthday, date)
    val months = period.getMonths
    if (i % 3 == 1 && newFoods.nonEmpty) {
      // add new food every three days
      val newFood = newFoods.head
      oldFoods = oldFoods :+ newFood
      newFoods = newFoods.drop(1)
      meatFoods = if (newFood.meat) meatFoods :+ newFood else meatFoods
      food2 = food1
      food1 = if (newFood.meat) pickVeg(oldFoods, food2) else newFood
      food3 = if (newFood.meat) Some(newFood) else None
    } else {
      food2 = food1
      food1 = pickVeg(oldFoods, food2)
      food3 = if (i % Math.max(Week / meatFoods.size, 2) == 0) {
        Some(meatQueue.next)
      } else None
    }
    val foo = s"$date,${months}月${days}天,$d,${food1.name},${food2.name},${food3.map(_.name).getOrElse("")}"
    println(foo)
    foo
  }

  import java.nio.file.{Files, Paths}

  import scala.collection.JavaConverters._

  Files.write(Paths.get("C:\\Users\\enjoy\\Desktop\\food.csv"), ("日期,月龄,天数,辅食1,辅食2,辅食3" +: outputs).asJava,
    Charset.forName("GBK"))
}
