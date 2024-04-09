package components

import com.almasb.fxgl.entity.component.Component
import com.almasb.fxgl.physics.PhysicsComponent
import com.almasb.fxgl.texture.{AnimatedTexture, AnimationChannel}
import javafx.scene.image.Image
import javafx.util.Duration

import foundation.given

class PlayerComponent extends Component {
    var physics: PhysicsComponent = _
    var texture: AnimatedTexture = _
    var animIdle: AnimationChannel = _
    var animWalk: AnimationChannel = _
    val moveHorizontalSpeed: Double = 200
    var jumpLeft: Int = 2
    
    val image: Image = Image("player.png")
    animIdle = AnimationChannel(image, 4, 32, 42, Duration.seconds(1), 1, 1)
    animWalk = AnimationChannel(image, 4, 32, 42, Duration.seconds(0.66), 0, 3)
    texture = AnimatedTexture(animIdle)
    texture.loop()
    
    
    override def onAdded(): Unit = {
        super.onAdded()
        entity.getTransformComponent.setScaleOrigin((16.0, 21.0))
        entity.getViewComponent.addChild(texture)
        physics.onGroundProperty().addListener((obs, o, n) => {
            if (n) {
                jumpLeft = 2
            }
        })
    }
    
    override def onUpdate(tpf: Double): Unit = {
        super.onUpdate(tpf)
        if (physics.isMovingX) {
            if (texture.getAnimationChannel != animWalk) {
                texture.loopAnimationChannel(animWalk)
            }
        } else {
            if (texture.getAnimationChannel != animIdle) {
                texture.loopAnimationChannel(animIdle)
            }
        }
    }
    
    def left(): Unit = {
        getEntity.setScaleX(-1)
        physics.setVelocityX(-moveHorizontalSpeed)
    }
    
    def right(): Unit = {
        getEntity.setScaleX(1)
        physics.setVelocityX(moveHorizontalSpeed)
    }
    
    def stop(): Unit = {
        physics.setVelocityX(0)
    }
    
    def jump(): Unit = {
        if (jumpLeft <= 0) {
            return
        }
        physics.setVelocityY(-300)
        jumpLeft -= 1
    }
}