package rainbow

import scala.scalajs.js
import js.annotation._

final case class LAB(l: Double, a: Double, b: Double) {
  def luminance = l
  def chroma = Math.sqrt(a * a + b * b)
  def hue = Math.atan2(b, a)
  def isGray = a == 0 && b == 0

  def toRGB: RGB = {
    val rgb = ColorConversion.labToRGB(l, a, b)
    RGB(rgb(0), rgb(1), rgb(2))
  }
  def toCSS = toRGB.toCSS
}
final case class RGB(r: Int, g: Int, b: Int) {
  def toCSS = s"rgb($r, $g, $b)"
}

@JSExport
object ColorConversion {
  import Math._

  val Xn = 95.047
  val Yn = 100.000
  val Zn = 108.883

  @inline final def finv(t: Double) = {
    // delta = 6.0/29.0
    val t3 = pow(t, 3)
    if (t3 > 0.008856451679035631) // t^3 > delta^3
      t3
    else
      0.12841854934601665 * (t - 0.13793103448275862) //3*d^2(t-4/29)
  }

  @inline final def csrgb(clin: Double) = {
    if (clin <= 0.0031308)
      12.92 * clin
    else
      1.055 * pow(clin, 0.4166666666666667) - 0.055
  }

  @JSExport
  @inline final def labToRGB(l: Double, a: Double, b: Double): Array[Int] = {
    // https://en.wikipedia.org/wiki/Lab_color_space#CIELAB-CIEXYZ_conversions
    var Y = (l + 16) / 116
    var X = a / 500 + Y
    var Z = Y - b / 200

    X = Xn * finv(X)
    Y = Yn * finv(Y)
    Z = Zn * finv(Z)

    // https://en.wikipedia.org/wiki/SRGB#The_forward_transformation_.28CIE_xyY_or_CIE_XYZ_to_sRGB.29

    X = X / 100 //X from 0 to  95.047      (Observer = 2°, Illuminant = D65)
    Y = Y / 100 //Y from 0 to 100.000
    Z = Z / 100 //Z from 0 to 108.883

    // linear rgb:
    var R = X * 3.2406 + Y * -1.5372 + Z * -0.4986
    var G = X * -0.9689 + Y * 1.8758 + Z * 0.0415
    var B = X * 0.0557 + Y * -0.2040 + Z * 1.0570

    R = csrgb(R)
    G = csrgb(G)
    B = csrgb(B)

    val Ri = (R * 255).toInt.max(0).min(255)
    val Gi = (G * 255).toInt.max(0).min(255)
    val Bi = (B * 255).toInt.max(0).min(255)
    Array(Ri, Gi, Bi)
  }
}
