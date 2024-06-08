package edu.cutie

import ui.UIController

object Entry {
    def main(args: Array[String]) : Unit = {
        val ui = UIController()
        ui.prepareUI()
        ui.run()
        println("Helllo, world!")
    }
}
