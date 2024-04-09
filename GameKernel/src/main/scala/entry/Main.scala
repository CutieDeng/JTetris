package entry

import com.almasb.fxgl.app.{GameApplication, GameSettings}
import com.almasb.fxgl.dsl.FXGL
import com.almasb.fxgl.entity.Entity
import foundation.given
import javafx.geometry.Point3D
import javafx.scene.input.KeyCode
import javafx.scene.paint.Color
import javafx.scene.shape.Rectangle

trait Controller:
    var pawn: Option[Entity] = None
    
object Main extends Controller {
}

class Main extends GameApplication {
    override def initSettings(gameSettings: GameSettings): Unit = {
        // handle
        gameSettings.setWidth(800)
        gameSettings.setHeight(600)
        gameSettings.setTitle("Impossible Eat")
        gameSettings.setVersion("0.1")
    }
    
    override def initGame() : Unit = {
        super.initGame()
        val f: Point3D = (100.2, 200.3, 35.2)
        println(f)
        val e = FXGL.entityBuilder().at(f).view(Rectangle(15, 35, Color.BLUE)).buildOrigin3DAndAttach()
        Main.pawn = Some(e)
    }
    
    override def initInput(): Unit = {
        super.initInput()
        FXGL.onKey(KeyCode.A, () => {
            Main.pawn match {
                case Some(p) => {
                    p.translateX(-5)
                }
                case _ => {}
            }
        })
        FXGL.onKey(KeyCode.D, () => {
            Main.pawn match {
                case Some(p) => {
                    p.translateX(5)
                }
                case _ => {}
            }
        })
    }
}

object Entry {
    def main(args: Array[String]) : Unit = {
        GameApplication.launch(classOf[Wind ], Array())
    }
}