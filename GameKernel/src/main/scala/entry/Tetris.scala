package entry

import com.almasb.fxgl.app.{GameApplication, GameSettings}
import com.almasb.fxgl.dsl.FXGL
import com.almasb.fxgl.entity.component.Component
import com.almasb.fxgl.entity.{Entity, EntityFactory, SpawnData, Spawns}
import javafx.scene.input.KeyCode
import javafx.scene.paint.Color
import javafx.scene.shape.Rectangle

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

case class GridWorld (row: Int, column: Int, step: Int, rowCoolDown: Double, columnCoolDown: Double) {
    
    var timeScale : Double = 1.0
    
    def Bounds : (Int, Int) = (row * step, column * step)
    
    var staticOccupications = Array.ofDim[Boolean](row, column)
    reset()
    
    def reset() : Unit = {
        staticOccupications.foreach( f => {
            for (i <- f.indices) {
                f(i) = false
            }
        } )
    }
    
    private def horizontalOperation(position: (Int, Int), abstractDirection: Int) : Option[(Int, Int)] = {
        if (abstractDirection == 1 || abstractDirection == -1) {
            val pGrid = ((position(0) + step - 1) / step, position(1) / step)
            if (pGrid(0) >= row * step) {
                return None
            }
            if (!(pGrid(1) + abstractDirection >= 0 && pGrid(1) + abstractDirection < column * step)) {
                return None
            }
            val target = (pGrid(0), pGrid(1) + abstractDirection)
            val o = staticOccupications(target(0))(target(1))
            if (!o) {
                Some((position(0), target(1) * step))
            } else {
                None
            }
        } else {
            throw RuntimeException()
        }
    }
    
    def leftOperation(p: (Int, Int)) = horizontalOperation(p, -1)
    def rightOperation(p: (Int, Int)) = horizontalOperation(p, 1)
    
    def lowOperation(p: (Int, Int)) : Option[(Int, Int)] = {
        val pGrid = (p(0) / step + 1, p(1) / step)
        try {
            val o = staticOccupications(pGrid(0))(pGrid(1))
            if (o) {
                None
            } else {
                Some((p(0) + 1, p(1)))
            }
        } catch {
            case e: RuntimeException => {
                None
            }
        }
    }
    
}

case class HotBlock(var row: Int, var col: Int, var entity: Entity, var vertTimeLeft: Double, var horiTimeLeft: Double)

class Tetris extends GameApplication {
    
    val gw = GridWorld(36, 12, 10, 0.019, 0.3)
    var hb = HotBlock(0, 0, null, 0.0, 0.0)
    val rand: Random = Random()
    
    override def initSettings(gameSettings: GameSettings): Unit = {
        gameSettings.setWidth(1600)
        gameSettings.setHeight(1200)
        gameSettings.setTitle("Tetris")
    }
    
    def flushHotBlock(rand: Random) : Unit = {
        val b = FXGL.spawn("Block")
        val x = rand.nextInt(gw.Bounds(1) - gw.step)
        hb.row = 0
        hb.col = x
        hb.entity = b
        hb.vertTimeLeft = 0.0
        hb.horiTimeLeft = 0.0
    }
    
    override def initGame(): Unit = {
        super.initGame()
        FXGL.getGameWorld.addEntityFactory(BlockEntityFactory())
        FXGL.entityBuilder().view(Rectangle(740, 380, Color.LIGHTBLUE)).at(100, 100).build()
        flushHotBlock(rand)
    }
    
    override def onUpdate(tpf: Double): Unit = {
        super.onUpdate(tpf)
        hb.horiTimeLeft -= tpf * gw.timeScale
        hb.vertTimeLeft -= tpf * gw.timeScale
        var c: Option[(Int, Int)] | Null = null
        while (hb.vertTimeLeft <= 0) {
            c = gw.lowOperation((hb.row, hb.col))
            c match {
                case Some((r, c)) => {
                    hb.row = r
                    hb.col = c
                }
                case _ => {}
            }
            hb.vertTimeLeft += gw.rowCoolDown
        }
        flushEntityPosition()
        c match {
            case None => {
                // set it as true ()
                gw.staticOccupications(hb.row / gw.step)(hb.col / gw.step) = true
                flushHotBlock(rand)
            }
            case _ => {}
        }
    }
    
    def flushEntityPosition() : Unit = {
        hb.entity.setX(hb.col * 3)
        hb.entity.setY(hb.row * 3)
    }
    
    override def initInput(): Unit = {
        super.initInput()
        val leftOp = (left: Boolean) => {
            if (hb.horiTimeLeft <= 0) {
                val c = if (left) gw.leftOperation((hb.row, hb.col)) else gw.rightOperation((hb.row, hb.col))
                c match {
                    case Some((r, c)) => {
                        hb.horiTimeLeft = gw.columnCoolDown
                        hb.row = r
                        hb.col = c
                        flushEntityPosition()
                    }
                    case _ => {}
                }
                println(s"$c")
            }
        }
        FXGL.onKeyDown(KeyCode.A, () => leftOp(true))
        FXGL.onKeyDown(KeyCode.LEFT, () => leftOp(true))
        FXGL.onKeyDown(KeyCode.D, () => leftOp(false))
        FXGL.onKeyDown(KeyCode.RIGHT, () => leftOp(false))
    }
    
    override def initPhysics(): Unit = {
        super.initPhysics()
        FXGL.getPhysicsWorld.setGravity(0, 1.0)
    }
}

class BlockEntityFactory extends EntityFactory {
    @Spawns("Block")
    def newBlock(data: SpawnData) : Entity = {
        FXGL.entityBuilder(data)
            .viewWithBBox(Rectangle(30, 30, Color.BLUE))
            .`with`(BlockComponent())
            .build()
    }
}

class BlockComponent extends Component

@main
def runnnnn() : Unit = {
    val f = TetrisGrids(3, 6)
    val fv = f.grids
    println(s"$fv")
    val xp = f((0, 0))
    println(s"$xp")
    // GameApplication.launch(classOf[Tetris], Array())
}

/// 方格信息
/// x: 列数
/// y: 行数