package com.evolution.bootcamp.homework.basics

object ClassesAndTraits {
  sealed trait Shape2D extends Located2D with Bounded2D with Movable2D {
    def area: Option[Double]
  }

  // Center of the shape
  sealed trait Located2D {
    def x: Double
    def y: Double
  }

  sealed trait Bounded2D {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  sealed trait Movable2D {
    def move(dx: Double, dy: Double): Movable2D
  }

  sealed trait Shape3D extends Located3D with Bounded3D with Movable3D {
    def surfaceArea: Option[Double]
    def volume: Option[Double]
  }

  // Center of the shape
  sealed trait Located3D {
    def x: Double
    def y: Double
    def z: Double
  }

  sealed trait Bounded3D {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
    def minZ: Double
    def maxZ: Double
  }

  sealed trait Movable3D {
    def move(dx: Double, dy: Double, dz: Double): Movable3D
  }

  final case class Point2D(x: Double, y: Double) extends Shape2D {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y

    def move(dx: Double, dy: Double): Point2D = Point2D(x + dx, y + dy)
    def area: Option[Double] = None
  }

  final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape2D {
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = centerX - radius
    override def maxX: Double = centerX + radius
    override def minY: Double = centerY - radius
    override def maxY: Double = centerY + radius

    def move(dx: Double, dy: Double): Circle = Circle(centerX + dx, centerY + dy, radius)
    def area: Option[Double] = Some(Math.PI * Math.pow(radius, 2))
  }

  final case class Rectangle(leftBottomX: Double, leftBottomY: Double, rightTopX: Double, rightTopY: Double) extends Shape2D {
    override def x: Double = rightTopX - leftBottomX / 2
    override def y: Double = rightTopY - leftBottomY / 2
    override def minX: Double = leftBottomX
    override def maxX: Double = rightTopX
    override def minY: Double = leftBottomY
    override def maxY: Double = rightTopY

    def move(dx: Double, dy: Double): Rectangle = Rectangle(leftBottomX + dx, rightTopX + dx, leftBottomY + dy, rightTopY + dy)
    def area: Option[Double] = Some((rightTopX - leftBottomX) * (rightTopY - leftBottomY))
  }

  final case class Square(leftBottomX: Double, leftBottomY: Double, rightTopX: Double, rightTopY: Double) extends Shape2D {
    override def x: Double = rightTopX - leftBottomX / 2
    override def y: Double = rightTopY - leftBottomY / 2
    override def minX: Double = leftBottomX
    override def maxX: Double = rightTopX
    override def minY: Double = leftBottomY
    override def maxY: Double = rightTopY

    def move(dx: Double, dy: Double): Square = Square(leftBottomX + dx, rightTopX + dx, leftBottomY + dy, rightTopY + dy)
    def area: Option[Double] = Some((rightTopX - leftBottomX) * (rightTopY - leftBottomY))
  }

  final case class Triangle(x1: Double, y1: Double, x2: Double, y2: Double, x3: Double, y3: Double) extends Shape2D {
    val xCoordinates = Vector(x1, x2, x3)
    val yCoordinates = Vector(y1, y2, y3)
    override def x: Double = (x1 + x2 + x3) / 3
    override def y: Double = (y1 + y2 + y3) / 3
    override def minX: Double = xCoordinates.min
    override def maxX: Double = xCoordinates.max
    override def minY: Double = yCoordinates.min
    override def maxY: Double = yCoordinates.max

    def move(dx: Double, dy: Double): Triangle = Triangle(x1 + dx, y1 + dy, x2 + dx, y2 + dy, x3 + dx, y3 + dy)
    def area: Option[Double] = Some((x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2)) / 2)
  }

  final case class Sphere(centerX: Double, centerY: Double, centerZ: Double, radius: Double) extends Shape3D {
    override def x: Double = centerX
    override def y: Double = centerY
    override def z: Double = centerZ
    override def minX: Double = centerX - radius
    override def maxX: Double = centerX + radius
    override def minY: Double = centerY - radius
    override def maxY: Double = centerY + radius
    override def minZ: Double = centerZ - radius
    override def maxZ: Double = centerZ + radius

    def move(dx: Double, dy: Double, dz: Double): Sphere = Sphere(centerX + dx, centerY + dy, centerZ + dz, radius)
    override def surfaceArea: Option[Double] = Some(4 * Math.PI * Math.pow(radius, 2))
    override def volume: Option[Double] = Some((4 * Math.PI * Math.pow(radius, 3)) / 3)
  }

  final case class Point3D(x: Double, y: Double, z: Double) extends Shape3D {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def minZ: Double = z
    override def maxZ: Double = z

    def move(dx: Double, dy: Double, dz: Double): Point3D = Point3D(x + dx, y + dy,z + dz)
    override def surfaceArea: Option[Double] = None
    override def volume: Option[Double] = None
  }

  final case class Cube(leftBottomBaseX: Double, leftBottomBaseY: Double, rightTopBaseX: Double,
                        rightTopBaseY: Double, bottomBaseZ: Double, topBaseZ: Double)
    extends Shape3D {
    override def x: Double = (rightTopBaseX - leftBottomBaseX) / 2
    override def y: Double = (rightTopBaseY - leftBottomBaseY) / 2
    override def z: Double = (topBaseZ - bottomBaseZ) / 2
    override def minX: Double = leftBottomBaseX
    override def maxX: Double = rightTopBaseX
    override def minY: Double = leftBottomBaseY
    override def maxY: Double = rightTopBaseY
    override def minZ: Double = bottomBaseZ
    override def maxZ: Double = topBaseZ

    def move(dx: Double, dy: Double, dz: Double): Cube =
      Cube(leftBottomBaseX + dx, leftBottomBaseY + dy, rightTopBaseX + dx, rightTopBaseY + dy, bottomBaseZ + dz, topBaseZ + dz)
    override def surfaceArea: Option[Double] = Some(6 * Math.pow(rightTopBaseX - leftBottomBaseX, 2))
    override def volume: Option[Double] = Some(Math.pow(rightTopBaseX - leftBottomBaseX, 3))
  }

  final case class Cuboid(leftBottomBaseX: Double, leftBottomBaseY: Double, rightTopBaseX: Double,
                          rightTopBaseY: Double, bottomBaseZ: Double, topBaseZ: Double)
    extends Shape3D {
    override def x: Double = (rightTopBaseX - leftBottomBaseX) / 2
    override def y: Double = (rightTopBaseY - leftBottomBaseY) / 2
    override def z: Double = (topBaseZ - bottomBaseZ) / 2
    override def minX: Double = leftBottomBaseX
    override def maxX: Double = rightTopBaseX
    override def minY: Double = leftBottomBaseY
    override def maxY: Double = rightTopBaseY
    override def minZ: Double = bottomBaseZ
    override def maxZ: Double = topBaseZ

    def move(dx: Double, dy: Double, dz: Double): Cuboid =
      Cuboid(leftBottomBaseX + dx, leftBottomBaseY + dy, rightTopBaseX + dx, rightTopBaseY + dy, bottomBaseZ + dz, topBaseZ + dz)
    override def surfaceArea: Option[Double] =
      Some(2 * ((rightTopBaseX - leftBottomBaseX) * (rightTopBaseY - leftBottomBaseY) +
        (rightTopBaseX - leftBottomBaseX) * (topBaseZ - bottomBaseZ) +
        (rightTopBaseY - leftBottomBaseY) * (topBaseZ - bottomBaseZ)))
    override def volume: Option[Double] =
      Some((rightTopBaseX - leftBottomBaseX) * (rightTopBaseY - leftBottomBaseY) * (topBaseZ - bottomBaseZ))
  }

  // An ordinary triangle in three-dimensional space
  final case class Triangle3D(x1: Double, y1: Double, z1: Double,
                              x2: Double, y2: Double, z2: Double,
                              x3: Double, y3: Double, z3: Double)
    extends Shape3D{
    val xCoordinates = Vector(x1, x2, x3)
    val yCoordinates = Vector(y1, y2, y3)
    val zCoordinates = Vector(z1, z2, z3)
    override def x: Double = (x1 + x2 + x3) / 3
    override def y: Double = (y1 + y2 + y3) / 3
    override def z: Double = (z1 + z2 + z3) / 3
    override def minX: Double = xCoordinates.min
    override def maxX: Double = xCoordinates.max
    override def minY: Double = yCoordinates.min
    override def maxY: Double = yCoordinates.max
    override def minZ: Double = zCoordinates.min
    override def maxZ: Double = zCoordinates.max

    def move(dx: Double, dy: Double, dz: Double): Triangle3D =
      Triangle3D(x1 + dx, y1 + dy, z1 + dz, x2 + dx, y2 + dy, z2 + dz, x3 + dx, y3 + dy, z3 + dz)
    override def surfaceArea: Option[Double] = ???
    override def volume: Option[Double] = None
  }

}
