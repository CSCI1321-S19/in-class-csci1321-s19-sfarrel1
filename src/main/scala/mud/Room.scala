package mud

class Room(
  name: String,
  desc: String,
  private var items: List[Item],
  exits: Array[Int]) {
  
  def description(): String = {
    val dirs = "north south east west up down".split(" ")
    val exitPrint = exits.zip(dirs).filter(_._1 != -1).map(_._2).mkString(",")
    val names = items.map(_.name)
    name+"\n"+desc+"\n" + "items:" + names.mkString(", ") + "\n" + "exits:" + exitPrint
  }

  def getExit(dir: Int): Option[Room] = {
        //println("Im in the getExit method")
    if (exits(dir) == -1) None
    //else if (dir > exits.length) None
    else Some(Room.rooms(exits(dir)))

  }

  def getItem(itemName: String): Option[Item] ={
    val itemNames = itemName.trim
    println(itemNames)
    val names = items.map(_.name)
    println(names)
    if (names.contains(itemNames)){
      println("you found and item")
      Some(items(names.indexOf(itemNames)))
      val dropIndex: Int = names.indexOf(itemNames)
      val itemReturn : Option[Item] = Some(items(names.indexOf(itemNames)))
      val itemIndexs = items.zipWithIndex
      val itemsPrint = items.zip(itemIndexs).filter(_._1 != items(dropIndex)).map(_._1)
     // println("this is itemsPrint");print(itemsPrint)
      items = itemsPrint
      itemReturn
    }
    else {
      println("No item to grab"); None   
    }
  }

  def dropItem(item: Item): Unit = {
    items = item :: items
  }
}

object Room {
  val rooms = readRooms()

  def readRooms(): Array[Room] = {
    val roomXML = xml.XML.loadFile("map.xml")
    val rooms = (roomXML \ "location").map(fromXML)
    //println(rooms)
    rooms.toArray
  }
  
  def fromXML(n: xml.Node): Room = {
    //println("im in te XML NOdE method")
    val name = (n \ "@name").text
    val desc = (n \ "description").text.trim()
    val items = (n \ "item").map(Item.fromXML).toList
    val exits = (n \ "exits").text.split(",").map(_.trim.toInt)
    new Room(name, desc, items, exits)
  }
}

