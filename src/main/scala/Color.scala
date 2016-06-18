package rainbow

import scala.scalajs.js
import js.annotation._

trait Color {
  def toRGB = ???
  def toLAB = ???
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
  @inline final def labToRGB(l: Double, a: Double, b: Double): Array[Double] = {
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

    R = (R * 255) //.max(0).min(255) //0.max(round(R * 255).toInt.min(255))
    G = (G * 255) //.max(0).min(255) //0.max(round(G * 255).toInt.min(255))
    B = (B * 255) //.max(0).min(255) //0.max(round(B * 255).toInt.min(255))
    Array(R, G, B)
  }
}
