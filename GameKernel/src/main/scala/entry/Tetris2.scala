package entry

import com.almasb.fxgl.app.{GameApplication, GameSettings}
import com.almasb.fxgl.dsl.FXGL
import com.almasb.fxgl.entity.Entity
import com.almasb.fxgl.entity.component.Component
import javafx.beans.property.StringProperty
import javafx.scene.control.{Button, Label}
import javafx.scene.input.KeyCode
import javafx.scene.layout.{HBox, VBox}
import javafx.scene.paint.Color
import javafx.scene.shape.Rectangle
import javafx.scene.text.Font

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class Tetris2 {
    
}

case class Timer(var duration: Double, var currentTime: Double = 0.0) {
    val actionList: scala.collection.mutable.ArrayBuffer[() => Unit] = ArrayBuffer[() => Unit]()
    def onUpdate(tpf: Double) : Unit = {
        currentTime += tpf
        for (_ <- 0 until 3) {
            if (currentTime >= duration) {
                currentTime -= duration
                for (a <- actionList) {
                    a()
                }
            } else {
                return
            }
        }
    }
}

val DEFAULT_DURATION : Double = 0.25
var nowSpeed : Double = 0.25

case class OccupiedMatrix$4x4[T](value: List[T]) {
    def rotateInverse(len: Int = 4) : OccupiedMatrix$4x4[T] = {
        val lAppender = List.newBuilder[T]
        for (i <- 0 until 4) {
            for (j <- 0 until 4) {
                if (i < len && j < len) {
                    lAppender += value(j * 4 + (len - 1) - i)
                } else {
                    lAppender += this(i, j)
                }
            }
        }
        OccupiedMatrix$4x4(lAppender.result())
    }
    
    def normalize(zero: T): OccupiedMatrix$4x4[T] = {
        var rowIdx: Int = 0
        var thisLineEmpty = true
        for (i <- 0 until 4) {
            if (thisLineEmpty) {
                for (j <- 0 until 4) {
                    if (this (i, j) != zero) {
                        thisLineEmpty = false
                    }
                }
                if (thisLineEmpty) {
                    rowIdx += 1
                }
            }
        }
        var colIdx: Int = 0
        var thisColumnEmpty = true
        for (j <- 0 until 4) {
            if (thisColumnEmpty) {
                for (i <- 0 until 4) {
                    if (this (i, j) != zero) {
                        thisColumnEmpty = false
                    }
                }
                if (thisColumnEmpty) {
                    colIdx += 1
                }
            }
        }
        return truncate(rowIdx, colIdx, zero)
    }
    
    def truncate(row: Int, column: Int, zero: T) : OccupiedMatrix$4x4[T] = {
        if (row == 0 && column == 0) {
            return this
        }
        if (row != 0) {
            val buf = List.newBuilder[T]
            for (i <- row until 4) {
                for (j <- 0 until 4) {
                    buf += this(i, j)
                }
            }
            for (_ <- 0 until row) {
                for (_ <- 0 until 4) {
                    buf += zero
                }
            }
            val n = OccupiedMatrix$4x4(buf.result())
            return n.truncate(0, column, zero)
        }
        if (column != 0) {
            val buf = List.newBuilder[T]
            for (i <- 0 until 4) {
                for (j <- column until 4) {
                    buf += this (i, j)
                }
                for (j <- 0 until column) {
                    buf += zero
                }
            }
            val n = OccupiedMatrix$4x4(buf.result())
            return n
        }
        throw RuntimeException()
    }
    
    def apply(idx: (Int, Int)) : T = {
        if (idx(0) >= 0 && idx(0) < 4) {
            if (idx(1) >= 0 && idx(1) < 4) {
                return value(idx(0) * 4 + idx(1))
            }
        }
        throw IndexOutOfBoundsException(s"$idx out of bounds (4, 4)")
    }
}

object OccupiedMatrix$4x4 {
    def empty[T]() : OccupiedMatrix$4x4[Option[T]] = {
        OccupiedMatrix$4x4( List.fill(16)(None) )
    }
    private def randList$4x4(rand: Random) : List[Boolean] = {
        val builder = List.newBuilder[Boolean]
        for (_ <- 0 until 16) {
            builder += rand.nextBoolean() && rand.nextBoolean()
        }
        builder.result()
    }
    private def dfs(idx: (Int, Int), target: OccupiedMatrix$4x4[Boolean], mark: Array[Boolean] = Array.fill(16)(false)) : Int = {
        val ops = Array(
            (idx(0) - 1, idx(1)),
            (idx(0), idx(1) - 1),
            (idx(0) + 1, idx(1)),
            (idx(0), idx(1) + 1)
        )
        val t = target(idx)
        if (mark(idx(0) * 4 + idx(1))) {
            return 0
        }
        var add = if (t) { 1 } else { 0 }
        mark(idx(0) * 4 + idx(1)) = true
        if (add == 0) {
            return add
        }
        for (o <- ops) {
            try {
                add += dfs(o, target, mark)
            } catch {
                case e: IndexOutOfBoundsException => {
                
                }
            }
        }
        return add
    }
    def randMatrix(rand: Random) : OccupiedMatrix$4x4[Boolean] = {
        while (true) {
            val l = randList$4x4(rand)
            val l2 = OccupiedMatrix$4x4(l)
            val idx = l2.value.indexOf(true)
            if (idx != -1) {
                val cnt0 = dfs((idx / 4, idx % 4), l2)
                val cnt1 = l2.value.count(p => p)
                if (cnt0 == cnt1) {
                    if (cnt0 != 1) {
                        val rst0 = OccupiedMatrix$4x4(l)
                        return rst0
                    }
                }
            }
        }
        throw RuntimeException()
    }
    def toIndexer[T](matrix: OccupiedMatrix$4x4[T], filter: (T) => Boolean, starter: (Int, Int)) : OccupiedMatrix$4x4[Option[(Int, Int)]] = {
        val a = List.newBuilder[Option[(Int, Int)]]
        for (i <- 0 until 4) {
            for (j <- 0 until 4) {
                if (filter (matrix((i, j)))) {
                    a += Some( (starter(0) + j, starter(1) + i) )
                } else {
                    a += None
                }
            }
        }
        OccupiedMatrix$4x4(a.result())
    }
}

class GameControllerComponent(var gridSize: Double, var gridsLocation: (Double, Double)) extends Component {
    
    val t: Timer = Timer(DEFAULT_DURATION)
    t.actionList += (() => {
        val v = currentGridLocations(idx).value
        val m: List[Option[(Int, Int)]] = v.map({ case None => None; case Some(a) => Some((a(0), a(1) + 1)) })
        val m2 = m.flatten
        val m3 = m2.filter(v => gameLogicGrids(v) match { case None | Some(Some(_)) => true; case _ => false })
        val nEmpty = m3.nonEmpty
        var gameOver : Boolean = false
        if (nEmpty) {
            v.zip(currentGrids(idx).value).foreach({
                case (Some(a), Some(b)) => {
                    gameLogicGrids(a) = Some(b)
                    if (a(1) < 4) {
                        gameOver = true
                    }
                }
                case (None, None) => {
                }
            })
            if (!gameOver) {
                randomEntity()
                t.currentTime = 0
                t.duration = nowSpeed
            } else {
                fail()
            }
        } else {
            for (i <- 0 until 4) {
                val v = currentGridLocations(i).value
                val m: List[Option[(Int, Int)]] = v.map({ case None => None; case Some(a) => Some((a(0), a(1) + 1)) })
                currentGridLocations = currentGridLocations.updated(i, OccupiedMatrix$4x4(m))
            }
            flush()
        }
    })
    t.actionList += (() => {
        var shouldLoop : Boolean = true
        var done: Boolean = false
        var oneLine : Int = 0
        while (shouldLoop) {
            shouldLoop = false
            var i = 0
            while (i < gameLogicGrids.y && !shouldLoop) {
                val c = gameLogicGrids.checkRow(i)
                if (c) {
                    oneLine += 1
                    val (a, b) = gameLogicGrids.removeRowNewly(i)
                    gameLogicGrids.grids = a
                    done = true
                    b.foreach({
                        case Some(e) => {
                            e.removeFromWorld()
                            shouldLoop = true
                        }
                        case _ => {}
                    })
                }
                i += 1
            }
        }
        if (oneLine != 0) {
            val origin = pointBoxProperty.getValue.toIntOption.getOrElse(0)
            val nowV = origin + oneLine
            pointBoxProperty.setValue(nowV.toString)
            nowSpeed = 0.25 - (nowV / 7) * 0.04
        }
        if (done) {
            totalFlush()
        }
    })
    
    val gameLogicGrids : TetrisGrids = TetrisGrids(7, 17)
    val rand: Random = Random()
    
    var currentGrids: List[OccupiedMatrix$4x4[Option[Entity]]] = null
    var currentGridLocations: List[OccupiedMatrix$4x4[Option[(Int, Int)]]] = null
    var idx: Int = 0
    
    var nextGrids: OccupiedMatrix$4x4[Option[Entity]] | Null = null
    
    override def onUpdate(tpf: Double): Unit = {
        super.onUpdate(tpf)
        t.onUpdate(tpf)
    }
    
    def fail() : Unit = {
        idx = 0
        currentGrids = List.fill(4)(OccupiedMatrix$4x4.empty())
        currentGridLocations = List.fill(4)(OccupiedMatrix$4x4.empty())
        failureLabel.setVisible(true)
        val service = FXGL.getNotificationService
        service.setTextColor(Color.RED)
        service.setBackgroundColor(Color.DARKGREY)
        service.pushNotification("你输了，按 Enter 再来一次")
    }
    
    val pointBox = HBox()
    var pointBoxProperty: StringProperty = null
    var failureLabel : Label = null
    
    override def onAdded(): Unit = {
        super.onAdded()
        FXGL.entityBuilder()
            .view(Rectangle(gameLogicGrids.x * gridSize + 34, (gameLogicGrids.y - 4) * gridSize + 34, Color.LIGHTCYAN))
            .at(gridsLocation(0) - 16, gridsLocation(1) - 18, 1)
            .buildOrigin3DAndAttach()
        randomEntity()
        pointBox.setSpacing(15)
        val l0 = Label("Points: ")
        l0.setFont(Font.font(28))
        pointBox.getChildren.add(l0)
        val pointLabel = Label()
        pointLabel.setFont(Font.font(27))
        val pointProperty = pointLabel.textProperty()
        pointBoxProperty = pointProperty
        pointBox.getChildren.add(pointLabel)
        failureLabel = Label("You fail")
        failureLabel.setFont(Font.font(24))
        failureLabel.setVisible(false)
        FXGL.addUINode(failureLabel, gameLogicGrids.x * gridSize + 102, 140)
        FXGL.addUINode(pointBox, gameLogicGrids.x * gridSize + 99, 101)
        FXGL.onKeyDown(KeyCode.ENTER, () => {
            if (failureLabel.isVisible) {
                restart()
            }
        })
        FXGL.onKeyDown(KeyCode.A, () => {
            val v = currentGridLocations(idx).value
            val m: List[Option[(Int, Int)]] = v.map({ case None => None; case Some(a) => Some((a(0) - 1, a(1))) })
            val m2 = m.flatten
            val m3 = m2.filter(v => gameLogicGrids(v) match {
                case None | Some(Some(_)) => true;
                case _ => false
            })
            val nEmpty = m3.nonEmpty
            if (!nEmpty) {
                for (i <- 0 until 4) {
                    val v = currentGridLocations(i).value
                    val m: List[Option[(Int, Int)]] = v.map({ case None => None; case Some(a) => Some((a(0) - 1, a(1))) })
                    currentGridLocations = currentGridLocations.updated(i, OccupiedMatrix$4x4(m))
                }
                flush()
            }
        })
        FXGL.onKeyDown(KeyCode.D, () => {
            val v = currentGridLocations(idx).value
            val m: List[Option[(Int, Int)]] = v.map({ case None => None; case Some(a) => Some((a(0) + 1, a(1))) })
            val m2 = m.flatten
            val m3 = m2.filter(v => gameLogicGrids(v) match {
                case None | Some(Some(_)) => true;
                case _ => false
            })
            val nEmpty = m3.nonEmpty
            if (!nEmpty) {
                for (i <- 0 until 4) {
                    val v = currentGridLocations(i).value
                    val m: List[Option[(Int, Int)]] = v.map({ case None => None; case Some(a) => Some((a(0) + 1, a(1))) })
                    currentGridLocations = currentGridLocations.updated(i, OccupiedMatrix$4x4(m))
                }
                flush()
            }
        })
        FXGL.onKeyDown(KeyCode.S, () => {
            if (nowSpeed > 0.1) {
                val perc = t.currentTime / t.duration
                t.duration = 0.1
                t.currentTime = perc * t.duration
            }
        })
        FXGL.onKeyDown(KeyCode.W, () => {
            val nextIdx = if (idx + 1 == 4) 0 else idx + 1
            val nxtLocationNotValid = currentGridLocations(nextIdx).value.flatten.exists(l => gameLogicGrids(l) match {
                case None | Some(Some(_)) => true
                case _ => false
            })
            if (!nxtLocationNotValid) {
                idx = nextIdx
                flush()
            }
        })
        
    }
    
    def randomEntity() : Unit = {
        
        val xPos = rand.nextInt(gameLogicGrids.x - 3)
        val matrix = OccupiedMatrix$4x4.randMatrix(rand).normalize(false)
        val color = Color.color(rand.nextDouble(), rand.nextDouble(), rand.nextDouble())
        var matrix2 = OccupiedMatrix$4x4(matrix.value.map(v => {
            if (v) {
                Some(
                    FXGL.entityBuilder()
                        .view(Rectangle(gridSize, gridSize, color))
                        .buildOrigin3DAndAttach()
                )
            } else {
                None
            }
        }))
        
        nextGrids match {
            case null => {
                nextGrids = matrix2
                return randomEntity()
            }
            case _ => {}
        }
        var cur = nextGrids
        nextGrids = matrix2
        matrix2 = cur
        
        val a = List.newBuilder[OccupiedMatrix$4x4[Option[Entity]]]
        val b = List.newBuilder[OccupiedMatrix$4x4[Option[(Int, Int)]]]
        a += matrix2
        b += OccupiedMatrix$4x4.toIndexer(matrix2, { case Some(_) => true; case None => false }, (xPos, 0))
        for (_ <- 0 until 3) {
            matrix2 = matrix2.rotateInverse().normalize(None)
            a += matrix2
            b += OccupiedMatrix$4x4.toIndexer(matrix2, { case Some(_) => true; case None => false }, (xPos, 0))
        }
        idx = 0
        currentGrids = a.result()
        currentGridLocations = b.result()
        flush()
    }
    
    def flush() : Unit = {
        val grids = currentGrids(idx)
        val locations = currentGridLocations(idx)
        for (x <- 0 until 16) {
            grids.value(x) match {
                case Some(v) => {
                    val v2 = locations.value(x) match { case Some(v2) => v2 }
                    v.setPosition3D(
                        gridsLocation(0) + v2(0) * gridSize,
                        gridsLocation(1) + (v2(1) - 4) * gridSize,
                        0
                    )
                }
                case _ => {}
            }
        }
        val baseX = gridsLocation(0) + gameLogicGrids.x * gridSize + gridSize * 1.24
        val baseY = gridsLocation(1) + (gameLogicGrids.y - 10) * gridSize + gridSize * 0.2
        nextGrids match {
            case g: OccupiedMatrix$4x4[Option[Entity]] => {
                g.value.zipWithIndex.foreach({ case (Some(value), idx) => {
                    val xIdx = idx % 4
                    val yIdx = idx / 4
                    value.setPosition3D(baseX + xIdx * gridSize, baseY + yIdx * gridSize, 0)
                }
                case _ => {}
                })
            }
        }
    }
    
    def totalFlush() : Unit = {
        for (x <- 0 until gameLogicGrids.x) {
            for (y <- 0 until gameLogicGrids.y) {
                val g = gameLogicGrids((x, y))
                g match {
                    case Some(Some(e)) => {
                        e.setPosition(
                            gridsLocation(0) + x * gridSize,
                            gridsLocation(1) + (y - 4) * gridSize
                        )
                    }
                    case Some(None) => {}
                }
            }
        }
    }
    
    def restart() : Unit = {
        failureLabel.setVisible(false)
        pointBoxProperty.setValue("")
        nextGrids.value.foreach({ case Some(value) => value.removeFromWorld(); case _ => {} })
        nextGrids = null
        gameLogicGrids.restart()
        t.currentTime = 0
        t.duration = DEFAULT_DURATION
        randomEntity()
    }
    
}

class Start extends GameApplication {
    
    override def initSettings(gameSettings: GameSettings): Unit = {
        gameSettings.setWidth(624)
        gameSettings.setHeight(725)
        gameSettings.setTitle("Tetris")
    }
    
    override def initGame(): Unit = {
        super.initGame()
        FXGL.entityBuilder().buildAndAttach().addComponent(GameControllerComponent(45, (62, 70)))
    }
    
}

object MyCall {
    def main(args: Array[String]) : Unit = {
        GameApplication.launch(classOf[Start], Array())    
    }
}

case class TetrisGrids(x: Int, y: Int) {
    var grids : Vector[Option[Entity]] =
    {
        val s = Vector.newBuilder[Option[Entity]]
        for (i <- 0 until (x * y)) {
            s += None
        }
        s.result()
    }
    def apply(idx: (Int, Int)) : Option[Option[Entity]] = {
        val (x0, y0) = idx
        if (!(x0 >= 0 && x0 < x)) {
            return None
        } else if (!(y0 >= 0 && y0 < y)) {
            return None
        } else {
            val i = x0 + y0 * x
            return Some( grids(i) )
        }
    }
    def update(idx: (Int, Int), value: Option[Entity]): Option[Option[Entity]] = {
        val ret = this(idx)
        ret match {
            case Some(_) => {
                val i = idx(0) + idx(1) * x
                grids = grids.updated(i, value)
            }
            case _ => {
                throw Exception(s"$idx is invalid for $value. ")
            }
        }
        ret
    }
    /// 检查当前行的格子是否满员 (?)
    def checkRow(rowId: Int) : Boolean = {
        if (rowId < 0 || rowId >= y) {
            throw RuntimeException(s"Invalid row index: $rowId")
        }
        for (j <- 0 until x) {
            val cwd = this(j, rowId)
            cwd match {
                case Some(f) => {
                    f match {
                        case Some(_) => {
                        }
                        case None => {
                            return false
                        }
                    }
                }
            }
        }
        return true
    }
    /// 移除某一行
    def removeRowNewly(rowId: Int) : (Vector[Option[Entity]], Vector[Option[Entity]]) = {
        if (rowId < 0 || rowId >= y) {
            throw RuntimeException()
        }
        var toRemoves = Vector.newBuilder[Option[Entity]]
        for (i <- 0 until x) {
            toRemoves += { this(i, rowId) match { case Some(f) => f } }
        }
        var elseEntities = Vector.newBuilder[Option[Entity]]
        for (i <- 0 until x) {
            elseEntities += None
        }
        for (j <- 0 until y) {
            if (j != rowId) {
                for (i <- 0 until x) {
                    elseEntities += { this(i, j) match { case Some(f) => f } }
                }
            }
        }
        return (elseEntities.result(), toRemoves.result())
    }
    def restart() : Unit = {
        grids.foreach({ case Some(e) => e.removeFromWorld(); case _ => {} })
        grids = grids.map(_ => None)
    }
}
