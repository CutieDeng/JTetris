package edu.cutie
package ui

import java.awt.event.{KeyAdapter, KeyEvent, KeyListener}
import java.util.concurrent.atomic.AtomicBoolean
import javax.swing.JFrame
import scala.collection.immutable.HashMap

case class Query(hashMap: HashMap[Int, AtomicBoolean]) extends KeyAdapter {
    override def keyPressed(e: KeyEvent): Unit = {
        super.keyPressed(e)
        hashMap.get(e.getKeyCode) match {
            case Some(value) => value.set(true)
            case _ => {}
        }
    }
    
    override def keyReleased(e: KeyEvent): Unit = {
        super.keyReleased(e)
        hashMap.get(e.getKeyCode) match {
            case Some(value) => value.set(false)
            case _ => {}
        }
    }
}

object Query {
    def everyKeyCodeInit(): Query = {
        val hashMap = HashMap.newBuilder[Int, AtomicBoolean]
        val fs = classOf[KeyEvent].getFields.filter(f => f.getName.startsWith("VK_")) // .filter(f => f.getType == Int.getClass)
        for (f <- fs) {
            val i = f.get(null).asInstanceOf[Int]
            hashMap.addOne((i, AtomicBoolean(false)))
        }
        Query(hashMap.result())
    }
}
