package mud

class Room(
  name: String,
  desc: String,
  private var items: List[Item],
  exits: Array[Int]) {
  
  def description(): String = {
    val dirs = "north south east west up down".split(" ")
    val exitPrint = exits.zip(dirs).filter(_._1 != -1).map(_._2).mkString(",")
    name+"\n"+desc+"\n" + "items:" + items.mkString(", ") + "\n" + "exits:" + exitPrint
  }

  def getExit(dir: Int): Option[Room] = {
        //println("Im in the getExit method")
    if (exits(dir) == -1) None
    //else if (dir > exits.length) None
    else Some(Room.rooms(exits(dir)))

  }

  def getItem(itemName: String): Option[Item] ={
    //println(items(0).toString())
    val names = items.map(_.toString())
    val nameIndex = names.filter(_.contains(itemName))
    
    //println(nameIndex)
    if (nameIndex.isEmpty) {
       None
    }
    else {
      //println(Some(items(names.indexOf(nameIndex(0)))))
      val dropIndex: Int = names.indexOf(nameIndex(0))
      //println(dropIndex)
      val itemReturn : Option[Item] = Some(items(names.indexOf(nameIndex(0))))
      
      val indexs = "0 1 2 3 4 5 6 7 8 9 10".split(" ")
      val itemsPrint = items.zip(indexs).filter(_._1 != items(dropIndex)).map(_._1)
      //println("this is itemsPrint");print(itemsPrint)
      items = itemsPrint //TODO
      //println(items)
      itemReturn
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

