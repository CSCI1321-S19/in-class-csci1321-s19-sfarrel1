package mud

class Player (
    val name: String,
    private var location: Room,
    private var inventory: List[Item]
    ){
  println(location.description())
  
  def processCommand(commands: String): Unit ={
    //- Parse and act on a command
    val command  = commands.toLowerCase().trim()
    if(command == "help"){
      println("""Select one of the following options:
1. n (north), s (south), e (east), w (west), u (up), d (down) - for movement
2. look - reprints room description
3. inv/inventory - lists the content of your inventory
4. get item - gets item from room and adds it to your inventory
5. drop item - drops item from inventory into the room
6. exit - leaves the game
7. help - reprints this help menu""")
    }
    else if (command == "look") {
      println(location.description())
    }
    else if (command == "inv" || command == "inventory") {
      println(inventoryListing)
    }
    else if (command == "n" || command =="s" || command =="e" || command =="w" || command =="u" || command =="d"){ 
      //println("move")
      move(command)
    }
    else if (command.take(3) == "get"){
      //println("go to get item method")
      val regex = "get ".r
      val itemNames = regex.replaceFirstIn(command, "")
      //println("item: " + itemNames)
      val newItem = (location.getItem(itemNames))
        newItem match{
          case Some(newItem) => addToInventory(newItem)
          case None => println("there was no item to grab")
        }
    }
    else if (command.take(4) == "drop"){
      //println("go to drop item method")
      val dropItem = getFromInventory(command)
      dropItem match{
          case Some(dropItem) => println("item removed from inventory")
          case None => println("there was no item to grab")
        }
      
    }
    else if (command == "exit"){
      println("exiting game")
    }
    else {
      println("Invalid instruction. For more information on valid instructions type 'help'")
    }
  }
  
  def getFromInventory(itemName: String): Option[Item]= {
    //- Pull an item out of the inventory (if it exists) and return it.
    val regex = "drop ".r
    val itemNs = regex.replaceFirstIn(itemName, "")
    
    val itemNames = itemNs.trim
    println(itemNames)
    val names = inventory.map(_.name)
    println(names)
    if (names.contains(itemNames)){
      println("you found and item")
      val dropIndex: Int = names.indexOf(itemNames)
      val itemReturn : Option[Item] = Some(inventory(names.indexOf(itemNames)))
      val itemIndexs = inventory.zipWithIndex
      val itemsPrint = inventory.zip(itemIndexs).filter(_._1 != inventory(dropIndex)).map(_._1)
     // println("this is itemsPrint");print(itemsPrint)
      inventory = itemsPrint
      //inventory.diff(List(inventory(dropIndex))) //TODO
      //go return the item
       itemReturn match{
          case Some(itemReturn) => location.dropItem(itemReturn)
          case None => println("there was no item to grab")
        }
      itemReturn
    }
    else {
      println("No item to grab"); None   
    }
    
    
    /*
    val names = inventory.map(_.toString())
    val nameIndex = names.filter(_.contains(itemNames))
    
    //println(nameIndex)
    if (nameIndex.isEmpty) {
       None
    }
    else {
      //println(Some(inventory(names.indexOf(nameIndex(0)))))
      val dropIndex: Int = names.indexOf(nameIndex(0))
      //println(dropIndex)
      val itemReturn : Option[Item] = Some(inventory(names.indexOf(nameIndex(0))))
      
      val itemIndexs = inventory.zipWithIndex
      val itemsPrint = inventory.zip(itemIndexs).filter(_._1 != inventory(dropIndex)).map(_._1)
     // println("this is itemsPrint");print(itemsPrint)
      inventory = itemsPrint
      //inventory.diff(List(inventory(dropIndex))) //TODO
      
     
      //go return the item
       itemReturn match{
          case Some(itemReturn) => location.dropItem(itemReturn)
          case None => println("there was no item to grab")
        }
      itemReturn
      }*/
  }
  
  def addToInventory(item: Item): Unit = {
    //- Add the given item to inventory.
    inventory = item :: inventory
  }
  
  def inventoryListing(): String = {
   if (inventory.isEmpty){
     "No items in Inventory"
   }
   else {
      //println(inventory(0).name)
      
      val x: Int = inventory.length
      var invArray = Array.fill(inventory.length+1)("Empty")
      invArray(0) = "Inventory:"+"\n\t"
       for(i <- 0 to (x-1)){
        invArray(i+1) = inventory(i).name + " - " + inventory(i).desc + "\n\t"
      }
      //println(invArray.mkString)
      invArray.mkString
    }
  }
  
  def move(dir: String): Unit ={
    //- Move the player in a particular direction if possible.
    //println("Im in class Player")
    //println(Player.moveDec(dir))
    
    val dirInt = Player.moveDec(dir)
    val room = location.getExit(dirInt).getOrElse(-1).toString()
    /*
    println("Im after location")
    println(room)
    print("here is room 0"); println(Room.rooms(0))
    print("here is location"); println(location)
    */

      //println("im in else state")
      val newLocation: Option[Room] = location.getExit(dirInt)
      //println(newLocation)
      newLocation match{
        case Some(newLocation) => location = newLocation
        case None => println("No room to exit to")
      }
    println(location.description())
    //println("im done with move method")
  }
  
}

object Player {
  def moveDec(dir: String): Int = {
   //println("Im in object Player")
   val room = dir match {
      case "n" => 0
      case "s" => 1
      case "e" => 2
      case "w" => 3
      case "u" => 4
      case "d" => 5
    }
    //println(room)
    room.toInt
  }

}





