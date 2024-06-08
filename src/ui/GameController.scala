package edu.cutie
package ui

import ui.GameController.{checkCollision, checkValidity, exactlyRemove, exactlySet, findLineFull, nextFall, nextHorizontalFall, removeRowsWithGravity}

import scala.annotation.internal.Child
import scala.collection.immutable.VectorBuilder

type ChildrenType = Vector[Vector[Boolean]]

class GameController (val columnCnt: Int, val rowCnt: Int, val fall: List[(Int, Int)], val children: ChildrenType) {
    def setNewFail(fall2: List[(Int, Int)]) : GameController = {
        assert(fall.isEmpty)
        assert(fall2.forall((c, r) => r < 0))
        GameController(columnCnt, rowCnt, fall2, children)
    }
    def down(): (GameController, Boolean) = {
        val fall2 = nextFall(fall)
        if (!checkValidity(fall2, columnCnt, rowCnt)) {
            return (fail(), false)
        }
        val col = checkCollision(children, fall, fall2)
        if (col.isEmpty) {
            val child2 = exactlySet(exactlyRemove(children, fall), fall2)
            return (GameController(columnCnt, rowCnt, fall2, child2), true)
        } else {
            return (fail(), false)
        }
    }
    def attemptHorizontal(v: Int) : GameController = {
        val fall2 = nextHorizontalFall(fall, v)
        if (!checkValidity(fall2, columnCnt, rowCnt)) {
            return this
        }
        val coli = checkCollision(children, fall, fall2)
        if (coli.isEmpty) {
            val child2 = exactlySet(exactlyRemove(children, fall), fall2)
            return GameController(columnCnt, rowCnt, fall2, child2)
        } else {
            return this
        }
    }
    private def fail() : GameController = {
        GameController(columnCnt, rowCnt, List.empty, children)
    }
    def autoRowRemove() : GameController = {
        val ignoreMovement = exactlyRemove(children, fall)
        val check = findLineFull(ignoreMovement)
        if (check.isEmpty) {
            return this
        } else {
            val removes = removeRowsWithGravity(ignoreMovement, check)
            // security issue
            val child2 = exactlySet(removes, fall)
            return GameController(columnCnt, rowCnt, fall, child2)
        }
    }
}

object GameController {
    def nextFall(fall: List[(Int, Int)]) : List[(Int, Int)] = {
        fall.map((c, r) => (c, r + 1))
    }
    def nextHorizontalFall(fall: List[(Int, Int)], h: Int) : List[(Int, Int)] = {
        fall.map((c, r) => (c + h, r))
    }
    def checkValidity(fall: List[(Int, Int)], columnCnt: Int, rowCnt: Int) : Boolean = {
        fall.forall((c, r) => c >= 0 && c < columnCnt && r < rowCnt)
    }
    def findLineFull(children: ChildrenType) : List[Int] = {
        var rst : List[Int] = List.empty
        val child2 = children.transpose
        for ((row, r) <- child2.zipWithIndex) {
            val b = row.forall { x => x }
            if (b) {
                rst = r :: rst
            }
        }
        rst
    }
    def removeRowsWithGravity(children: ChildrenType, removes: List[Int]) : ChildrenType = {
        val colCnt = children.length
        val child2 = children.transpose
        val rowCnt = child2.length
        val rstBuilder = Vector.newBuilder[Vector[Boolean]]
        val emptyInit = Vector.fill(colCnt){false}
        for (_ <- removes.indices) {
            rstBuilder.addOne(emptyInit)
        }
        for (r <- 0 until rowCnt) {
            if (!removes.contains(r)) {
                rstBuilder.addOne(child2(r))
            }
        }
        val rst = rstBuilder.result()
        assert(rst.length == child2.length)
        rst.transpose
    }
    def exactlyRemove(children: ChildrenType, mask: List[(Int, Int)]) : ChildrenType = {
        val rst = Vector.newBuilder[Vector[Boolean]]
        for ((elem, c) <- children.zipWithIndex) {
            if (mask.exists((c2, r) => c == c2)) {
                val innerRst = Vector.newBuilder[Boolean]
                for ((e, r) <- elem.zipWithIndex) {
                    if (mask.contains((c, r))) {
                        assert(e)
                        innerRst.addOne(false)
                    } else {
                        innerRst.addOne(e)
                    }
                }
                rst.addOne(innerRst.result())
            } else {
                rst.addOne(elem)
            }
        }
        rst.result()
    }
    def exactlySet(children: ChildrenType, mask: List[(Int, Int)]) : ChildrenType = {
        val rst = Vector.newBuilder[Vector[Boolean]]
        for ((elem, c) <- children.zipWithIndex) {
            if (mask.exists((c2, r) => c == c2)) {
                val innerRst = Vector.newBuilder[Boolean]
                for ((e, r) <- elem.zipWithIndex) {
                    if (mask.contains((c, r))) {
                        assert(!e)
                        innerRst.addOne(true)
                    } else {
                        innerRst.addOne(e)
                    }
                }
                rst.addOne(innerRst.result())
            } else {
                rst.addOne(elem)
            }
        }
        rst.result()
    }
    def checkCollision(children: ChildrenType, ignoreMask: List[(Int, Int)], newlyOccupied: List[(Int, Int)]): List[(Int, Int)] = {
        newlyOccupied
            .filter(!ignoreMask.contains(_))
            .filter((c, r) => r >= 0)
            .filter(children(_)(_))
    }
    def defaultLayout(colCnt: Int, rowCnt: Int) : ChildrenType = {
        Vector.fill(colCnt){ Vector.fill(rowCnt) { false } }
    }
}
