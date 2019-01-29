package mud

import io.StdIn._

object Main {
  def main(args: Array[String]): Unit = {
    println("Im in the main object")
    val player = new Player("Sean",Room.rooms(0), Nil)
    var option =""
    
    while(option != "exit") {
      option = readLine()
      player.processCommand(option)
    }
  }
 
}