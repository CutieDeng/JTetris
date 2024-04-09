package foundation

import javafx.geometry.{Point2D, Point3D}

given Conversion[(Double, Double, Double), Point3D] with {
    override def apply(x: (Double, Double, Double)): Point3D = Point3D(x(0), x(1), x(2))
}

given Conversion[(Double, Double), Point2D] with {
    override def apply(x: (Double, Double)): Point2D = Point2D(x(0), x(1)) 
}
