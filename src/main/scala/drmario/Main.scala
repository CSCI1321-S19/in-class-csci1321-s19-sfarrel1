package drmario

import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas

object Main extends JFXApp{
  val boardWidth = 8*30
  val boardHeight = 16*30
  stage = new JFXApp.PrimaryStage{
    title = "Dr. Mario"
    scene = new Scene(boardWidth, boardHeight){
    val canvas = new Canvas(boardWidth, boardHeight)
    //val gc = canvas.graphicsContent2D
      //content = Canvas
    }
  }
  
}