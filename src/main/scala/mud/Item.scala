package mud

case class Item (name: String, desc: String){
  //TODO
}

object Item {
  def fromXML(n: xml.Node): Item = {
    val name = (n \ "@name").text
    val desc = n.text.trim
    Item(name, desc)
  }
}