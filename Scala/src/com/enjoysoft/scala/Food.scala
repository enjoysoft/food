package com.enjoysoft.scala

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

  val testTxtSource = scala.io.Source.fromFile("C:\\Users\\enjoy\\Desktop\\food.txt")(scala.io.Codec.UTF8)

  val lines = testTxtSource.getLines()

  val foods: List[Food] = (for (line <- lines) yield Food(line.trim())).toList
  val Days = 30
  val Birthday = LocalDate.of(2017, 2, 16)
  var (newFoods, oldFoods) = foods.partition(_.add)
  var food1: Food = oldFoods.filterNot(_.meat).head
  var food2: Food = pickVeg(oldFoods, food1)
  var food3: Option[Food] = None

  def pickVeg(foods: List[Food], foodEx: Food): Food = {
    val veg = foods.filterNot(_.meat)
    pickFood(veg, foodEx)
  }

  def pickFood(foods: List[Food], foodEx: Food): Food = scala.util.Random.shuffle(foods.filterNot(_ == foodEx)).head

  for (i <- 1 to Days) {
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
      food2 = food1
      food1 = if (newFood.meat) pickVeg(oldFoods, food2) else newFood
      food3 = if (newFood.meat) Some(newFood) else None
    } else {
      food2 = food1
      food1 = pickVeg(oldFoods, food2)
      food3 = None
    }
    println(date, s"$months 月 $days 天", d, food1.name, food2.name, food3.map(_.name).getOrElse(""))
  }

}
