package pigment

import scala.scalajs.js
import js.annotation._
import Math._

final case class LAB(l: Double, a: Double, b: Double) {
  def luminance = l
  def chroma = sqrt(a * a + b * b)
  def hue = ((PI * 2) + atan2(b, a)) % (PI * 2)
  def isGray = a == 0 && b == 0

  def withChroma(c: Double) = {
    copy(a = a / chroma * c, b = b / chroma * c)
  }

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

object ColorDistance {

  def ciede2000(col1: LAB, col2: LAB): Double = ciede2000(col1.l, col1.a, col1.b, col2.l, col2.a, col2.b)
  def ciede2000(lab1l: Double, lab1a: Double, lab1b: Double, lab2l: Double, lab2a: Double, lab2b: Double): Double = {
    // ported from: https://github.com/THEjoezack/ColorMine/blob/master/ColorMine/ColorSpaces/Comparisons/CieDe2000Comparison.cs

    // Set weighting factors to 1
    val k_l = 1.0
    val k_c = 1.0
    val k_h = 1.0

    // Calculate Cprime1, Cprime2, Cabbar
    val c_star_1_ab = sqrt(lab1a * lab1a + lab1b * lab1b)
    val c_star_2_ab = sqrt(lab2a * lab2a + lab2b * lab2b)
    val c_star_average_ab = (c_star_1_ab + c_star_2_ab) / 2.0

    var c_star_average_ab_pot7 = c_star_average_ab * c_star_average_ab * c_star_average_ab
    c_star_average_ab_pot7 *= c_star_average_ab_pot7 * c_star_average_ab

    val g = 0.5 * (1.0 - sqrt(c_star_average_ab_pot7 / (c_star_average_ab_pot7 + 6103515625.0))) //25^7
    val a1_prime = (1.0 + g) * lab1a
    val a2_prime = (1.0 + g) * lab2a

    val c_prime_1 = sqrt(a1_prime * a1_prime + lab1b * lab1b)
    val c_prime_2 = sqrt(a2_prime * a2_prime + lab2b * lab2b)
    // Angles in Degree.
    val h_prime_1 = ((atan2(lab1b, a1_prime) * 180.0 / PI) + 360.0) % 360.0
    val h_prime_2 = ((atan2(lab2b, a2_prime) * 180.0 / PI) + 360.0) % 360.0

    val delta_l_prime = lab2l - lab1l
    val delta_c_prime = c_prime_2 - c_prime_1

    val h_bar = (h_prime_1 - h_prime_2).abs
    var delta_h_prime = if (c_prime_1 * c_prime_2 == 0.0) {
      0.0
    } else {
      if (h_bar <= 180.0) {
        h_prime_2 - h_prime_1
      } else if (h_bar > 180.0 && h_prime_2 <= h_prime_1) {
        h_prime_2 - h_prime_1 + 360.0
      } else {
        h_prime_2 - h_prime_1 - 360.0
      }
    }
    delta_h_prime = 2.0 * sqrt(c_prime_1 * c_prime_2) * sin(delta_h_prime * PI / 360.0)

    // Calculate CIEDE2000
    val l_prime_average = (lab1l + lab2l) / 2.0
    val c_prime_average = (c_prime_1 + c_prime_2) / 2.0

    // Calculate h_prime_average

    val h_prime_average =
      if (c_prime_1 * c_prime_2 == 0.0) {
        0.0
      } else {
        if (h_bar <= 180.0) {
          (h_prime_1 + h_prime_2) / 2.0
        } else if (h_bar > 180.0 && (h_prime_1 + h_prime_2) < 360.0) {
          (h_prime_1 + h_prime_2 + 360.0) / 2.0
        } else {
          (h_prime_1 + h_prime_2 - 360.0) / 2.0
        }
      }
    var l_prime_average_minus_50_square = l_prime_average - 50.0
    l_prime_average_minus_50_square *= l_prime_average_minus_50_square

    val s_l = 1.0 +
      ((0.015 * l_prime_average_minus_50_square) /
        sqrt(20.0 + l_prime_average_minus_50_square))
    val s_c = 1.0 + 0.045 * c_prime_average
    val t = 1.0 - 0.17 * cos((h_prime_average - 30.0).toRadians) +
      0.24 * cos((h_prime_average * 2.0).toRadians) +
      0.32 * cos((h_prime_average * 3.0 + 6.0).toRadians) -
      0.2 * cos((h_prime_average * 4.0 - 63.0).toRadians)
    val s_h = 1.0 + 0.015 * t * c_prime_average
    var h_prime_average_minus_275_div_25_square = (h_prime_average - 275.0) / 25.0
    h_prime_average_minus_275_div_25_square *= h_prime_average_minus_275_div_25_square
    val delta_theta = 30.0 * exp(-h_prime_average_minus_275_div_25_square)

    var c_prime_average_pot_7 = c_prime_average * c_prime_average * c_prime_average
    c_prime_average_pot_7 *= c_prime_average_pot_7 * c_prime_average
    val r_c = 2.0 * sqrt(c_prime_average_pot_7 / (c_prime_average_pot_7 + 6103515625.0))

    val r_t = -sin((2.0 * delta_theta).toRadians) * r_c

    val delta_l_prime_div_k_l_s_l = delta_l_prime / (s_l * k_l)
    val delta_c_prime_div_k_c_s_c = delta_c_prime / (s_c * k_c)
    val delta_h_prime_div_k_h_s_h = delta_h_prime / (s_h * k_h)

    val ciede2000 = sqrt(delta_l_prime_div_k_l_s_l * delta_l_prime_div_k_l_s_l +
      delta_c_prime_div_k_c_s_c * delta_c_prime_div_k_c_s_c +
      delta_h_prime_div_k_h_s_h * delta_h_prime_div_k_h_s_h +
      r_t * delta_c_prime_div_k_c_s_c * delta_h_prime_div_k_h_s_h)

    ciede2000
  }

  def tests() {
    def ciede2000_case(l1: Double, a1: Double, b1: Double, l2: Double, a2: Double, b2: Double, de: Double) {
      assert(round(ciede2000(l1, a1, b1, l2, a2, b2) * 10000.0) / 10000.0 == de)
    }

    // from http://www.ece.rochester.edu/~gsharma/ciede2000/dataNprograms/ciede2000testdata.txt

    ciede2000_case(50.0000, 2.6772, -79.7751, 50.0000, 0.0000, -82.7485, 2.0425)
    ciede2000_case(50.0000, 2.6772, -79.7751, 50.0000, 0.0000, -82.7485, 2.0425)
    ciede2000_case(50.0000, 3.1571, -77.2803, 50.0000, 0.0000, -82.7485, 2.8615)
    ciede2000_case(50.0000, 2.8361, -74.0200, 50.0000, 0.0000, -82.7485, 3.4412)
    ciede2000_case(50.0000, -1.3802, -84.2814, 50.0000, 0.0000, -82.7485, 1.0000)
    ciede2000_case(50.0000, -1.1848, -84.8006, 50.0000, 0.0000, -82.7485, 1.0000)
    ciede2000_case(50.0000, -0.9009, -85.5211, 50.0000, 0.0000, -82.7485, 1.0000)
    ciede2000_case(50.0000, 0.0000, 0.0000, 50.0000, -1.0000, 2.0000, 2.3669)
    ciede2000_case(50.0000, -1.0000, 2.0000, 50.0000, 0.0000, 0.0000, 2.3669)
    ciede2000_case(50.0000, 2.4900, -0.0010, 50.0000, -2.4900, 0.0009, 7.1792)
    ciede2000_case(50.0000, 2.4900, -0.0010, 50.0000, -2.4900, 0.0010, 7.1792)
    ciede2000_case(50.0000, 2.4900, -0.0010, 50.0000, -2.4900, 0.0011, 7.2195)
    ciede2000_case(50.0000, 2.4900, -0.0010, 50.0000, -2.4900, 0.0012, 7.2195)
    ciede2000_case(50.0000, -0.0010, 2.4900, 50.0000, 0.0009, -2.4900, 4.8045)
    ciede2000_case(50.0000, -0.0010, 2.4900, 50.0000, 0.0010, -2.4900, 4.8045)
    ciede2000_case(50.0000, -0.0010, 2.4900, 50.0000, 0.0011, -2.4900, 4.7461)
    ciede2000_case(50.0000, 2.5000, 0.0000, 50.0000, 0.0000, -2.5000, 4.3065)
    ciede2000_case(50.0000, 2.5000, 0.0000, 73.0000, 25.0000, -18.0000, 27.1492)
    ciede2000_case(50.0000, 2.5000, 0.0000, 61.0000, -5.0000, 29.0000, 22.8977)
    ciede2000_case(50.0000, 2.5000, 0.0000, 56.0000, -27.0000, -3.0000, 31.9030)
    ciede2000_case(50.0000, 2.5000, 0.0000, 58.0000, 24.0000, 15.0000, 19.4535)
    ciede2000_case(50.0000, 2.5000, 0.0000, 50.0000, 3.1736, 0.5854, 1.0000)
    ciede2000_case(50.0000, 2.5000, 0.0000, 50.0000, 3.2972, 0.0000, 1.0000)
    ciede2000_case(50.0000, 2.5000, 0.0000, 50.0000, 1.8634, 0.5757, 1.0000)
    ciede2000_case(50.0000, 2.5000, 0.0000, 50.0000, 3.2592, 0.3350, 1.0000)
    ciede2000_case(60.2574, -34.0099, 36.2677, 60.4626, -34.1751, 39.4387, 1.2644)
    ciede2000_case(63.0109, -31.0961, -5.8663, 62.8187, -29.7946, -4.0864, 1.2630)
    ciede2000_case(61.2901, 3.7196, -5.3901, 61.4292, 2.2480, -4.9620, 1.8731)
    ciede2000_case(35.0831, -44.1164, 3.7933, 35.0232, -40.0716, 1.5901, 1.8645)
    ciede2000_case(22.7233, 20.0904, -46.6940, 23.0331, 14.9730, -42.5619, 2.0373)
    ciede2000_case(36.4612, 47.8580, 18.3852, 36.2715, 50.5065, 21.2231, 1.4146)
    ciede2000_case(90.8027, -2.0831, 1.4410, 91.1528, -1.6435, 0.0447, 1.4441)
    ciede2000_case(90.9257, -0.5406, -0.9208, 88.6381, -0.8985, -0.7239, 1.5381)
    ciede2000_case(6.7747, -0.2908, -2.4247, 5.8714, -0.0985, -2.2286, 0.6377)
    ciede2000_case(2.0776, 0.0795, -1.1350, 0.9033, -0.0636, -0.5514, 0.9082)
  }
}
