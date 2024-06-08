package edu.cutie
package ui

import java.awt.Dimension
import java.awt.event.KeyEvent
import java.awt.geom.Dimension2D
import javax.swing.{JFrame, JPanel, WindowConstants}

class UIController {
    val columnCnt = 11
    val rowCnt = 21
    val frame: JFrame = JFrame()
    var controller = GameController(columnCnt, rowCnt, List.empty, GameController.defaultLayout(columnCnt, rowCnt))
    
    def prepareUI() : Unit = {
        // frame.setPreferredSize(Dimension(1000, 1000))
        frame.getContentPane.setPreferredSize(Dimension(1000, 1000))
        frame.pack()
        frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
        frame.setVisible(true)
    }
    
    def run() : Unit = {
        
        val query = Query.everyKeyCodeInit()
        frame.addKeyListener(query)
        
        val panel = frame.getContentPane
        val graphics = panel.getGraphics
        
        val xOffset = 340
        val yOffset = 40
        val gridSize = 35
        
        val oneFrameTime : Long = 1000000000L / 120
        
        val opCoolDown = 300
        val speedUp = 8
        var rest = 0L
        var timeM = System.currentTimeMillis()
        
        var horizonRest = 0L
        val horizonCoolDown = 140
        
        while (true) {
            val time = System.nanoTime()
            val timeC = System.currentTimeMillis()
            val dif = timeC - timeM
            timeM = timeC
            
            // check special keyCode
            var dif2 = dif
            if (query.hashMap(KeyEvent.VK_SPACE).get()) {
                dif2 *= speedUp
            }
            rest -= dif2
            if (rest <= 0) {
                rest = opCoolDown
                // check down ~
                controller = controller.down()(0)
                // prepare newly fail
                if (controller.fall.isEmpty) {
                    // handle it
                    controller = controller.setNewFail((columnCnt / 2 - 1, -1) :: Nil)
                }
                controller = controller.autoRowRemove()
            }
            
            horizonRest -= dif
            if (horizonRest <= 0) {
                val left = query.hashMap(KeyEvent.VK_LEFT).get() || query.hashMap(KeyEvent.VK_A).get()
                val right = query.hashMap(KeyEvent.VK_RIGHT).get() || query.hashMap(KeyEvent.VK_D).get()
                (left, right) match {
                    case (false, true) => {
                        controller = controller.attemptHorizontal(1)
                        horizonRest = horizonCoolDown
                    }
                    case (true, false) => {
                        controller = controller.attemptHorizontal(-1)
                        horizonRest = horizonCoolDown
                    }
                    case _ => {}
                }
            }
            
            // flush ui
            val graph2 = graphics.create()
            // graphics.clearRect(0, 0, 1000, 1000)
            for ((colElem, c) <- controller.children.zipWithIndex) {
                for ((elem, r) <- colElem.zipWithIndex) {
                    if (elem) {
                        graph2.drawRoundRect((xOffset + (c + 0.1) * gridSize).toInt, (yOffset + (r + 0.1) * gridSize).toInt,
                            (0.8 * gridSize).toInt, (0.8 * gridSize).toInt, 4, 4)
                        graph2.drawRoundRect((xOffset + (c + 0.04) * gridSize).toInt, (yOffset + (r + 0.04) * gridSize).toInt,
                            (0.92 * gridSize).toInt, (0.92 * gridSize).toInt, 2, 2)
                    } else {
                        graph2.clearRect(xOffset + c * gridSize, yOffset + r * gridSize, gridSize, gridSize)
                    }
                }
            }
            graph2.dispose()
            
            val timeE = System.nanoTime() - time
            if (timeE < oneFrameTime) {
                val dif = oneFrameTime - timeE
                Thread.sleep(dif / 1000000, (dif % 1000000).toInt)
            }
        }
    }
}
