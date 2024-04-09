package entry

import com.almasb.fxgl.app.{GameApplication, GameSettings}
import com.almasb.fxgl.dsl.FXGL
import com.almasb.fxgl.dsl.components.AutoRotationComponent
import com.almasb.fxgl.entity.Entity
import javafx.animation.{FadeTransition, ParallelTransition, PathTransition, TranslateTransition}
import javafx.application.Platform
import javafx.beans.property.SimpleDoubleProperty
import javafx.event.{ActionEvent, EventHandler}
import javafx.geometry.{Insets, Pos}
import javafx.scene.Cursor
import javafx.scene.control.{Button, Label, PasswordField, TextField}
import javafx.scene.layout.{Border, HBox, VBox}
import javafx.scene.paint.Color
import javafx.util.Duration

def createPopUp(content: String) : VBox = {
    val popUp = VBox(10)
    val yesBt = Button("Yes")
    val noBt = Button("No")
    yesBt.setOnAction((e) => {
        FXGL.getNotificationService.pushNotification("Yes")
    })
    noBt.setOnAction((e) => {
        popUp.setVisible(false)
    })
    popUp.getChildren.add(Label(content))
    popUp.getChildren.add(yesBt)
    popUp.getChildren.add(noBt)
    popUp
}

def createPopUpEntry(content: String) : Entity = {
    val p = createPopUp(content)
    val p2 = FXGL.entityBuilder().view(p).`with`(AutoRotationComponent()).buildAndAttach()
    FXGL.getGameTimer.runOnceAfter(() => {
        p2.removeFromWorld()
    }, Duration.seconds(5))
    p2
}

object InfoBar {
    val bar : VBox = VBox()
    bar.setPrefWidth(200)
    bar.setLayoutX(FXGL.getAppWidth - bar.getPrefWidth)
    bar.setLayoutY(20)
    FXGL.getGameScene.addUINode(bar)
    bar.setSpacing(5)
    
    def addNotification(message: String): Unit = {
        val m = Label(message)
        m.setStyle(
            """
              |-fx-background-color: #f4f4f4;
              |-fx-background-radius: 7;
              |-fx-padding: 2;
              |-fx-effect: dropshadow(gaussian, rgba(0,0,0,0.3), 10, 0, 0, 0);
              |""".stripMargin)
        val trans = TranslateTransition(Duration.seconds(0.5), m)
        trans.setFromX(-20)
        trans.setToX(0)
        val trans2 = FadeTransition(Duration.seconds(0.5), m)
        trans2.setFromValue(0)
        trans2.setToValue(1)
        val t = ParallelTransition(m, trans, trans2)
        t.setOnFinished(e => {
            FXGL.runOnce(() => {
                bar.getChildren.remove(m)
            }, Duration.seconds(5))
        })
        bar.getChildren.add(0, m)
        t.play()
        println(bar.getChildren)
    }
}

class Wind extends GameApplication {
    
    override def initSettings(gameSettings: GameSettings): Unit = {}
    
    override def initGame(): Unit = {
        super.initGame()
    }
    
    override def initUI() : Unit = {
        super.initUI()
        FXGL.getGameScene.setBackgroundColor(Color.LIGHTGRAY)
        FXGL.getGameScene.setCursor(Cursor.DEFAULT)
        val usernameField = TextField()
        val userPasswordField = PasswordField()
        val h0 = HBox(10, Label("Username: "), usernameField)
        h0.setAlignment(Pos.CENTER)
        val h1 = HBox(10, Label("Password: "), userPasswordField)
        h1.setAlignment(Pos.CENTER)
        val btn = Button("Submit")
        val act: EventHandler[ActionEvent] = (e: ActionEvent) => {
            println(s"You enter ${usernameField.getText}:${userPasswordField.getText}")
            if (userPasswordField.getText.trim.isEmpty) {
                // FXGL.getDialogService.showMessageBox("No passwd! ")
                // createPopUpEntry("Amaing. ")
                Platform.runLater(() => {
                    InfoBar.addNotification("Amazing")
                    FXGL.runOnce(() => {
                        InfoBar.addNotification("Amazing2")
                    }, Duration.seconds(1))
                })
            } else {
                val dp = SimpleDoubleProperty(0.0)
                val a : Runnable = () => {
                    while (dp.get() < 1) {
                        println("test1")
                        val r : Runnable = () => dp.set(dp.get() + 0.01)
                        FXGL.getExecutor.startAsyncFX(r).await()
                        Thread.sleep(100)
                    }
                }
                FXGL.getExecutor.execute(a)
                FXGL.getDialogService.showProgressBox("Hello", dp, () => {
                    println("End. ")
                    FXGL.getNotificationService.pushNotification("Hey, here. ")
                })
                FXGL.getNotificationService.pushNotification("Hey, here, first.  ")
            }
        }
        usernameField.setOnAction(_ => userPasswordField.requestFocus())
        userPasswordField.setOnAction(_ => btn.requestFocus())
        btn.setOnAction(act)
        val loginRoot = VBox(10, h0, h1, btn)
        loginRoot.setPrefSize(FXGL.getAppWidth, FXGL.getAppHeight)
        loginRoot.setAlignment(Pos.CENTER)
        
        FXGL.addUINode(loginRoot)
    }
}
