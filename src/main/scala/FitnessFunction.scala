package pigment

import Math._

case class FitnessFunction(terms: Seq[Term] = Nil) {
  def apply(c: ColorScheme) = terms.map(term => term(c)).sum
}

case class Term(goal: Goal, metric: Metric, target: Target, modifiers: Seq[Modifier] = Nil) {
  def apply(c: ColorScheme) = {
    val t = target(c)
    if (t.isEmpty) 0
    else {
      val result = metric(target(c))
      val modified = modifiers.foldLeft(result)((res, modifier) => modifier(res))
      goal(modified)
    }
  }

  def result(c: ColorScheme) = {
    val t = target(c)
    if (t.isEmpty) 0
    else metric(target(c))
  }
  override def toString = s"$goal $metric $target ${modifiers.mkString(",")}"
}

sealed trait Goal { def apply(x: Double): Double }
case object Maximize extends Goal { def apply(x: Double) = x }
case object Minimize extends Goal { def apply(x: Double) = -x }
case class Approximate(goal: Double) extends Goal { def apply(x: Double) = -((goal - x).abs) }

sealed trait Metric { def apply(xs: Seq[Double]): Double }
case object Min extends Metric { def apply(xs: Seq[Double]) = xs.min }
case object Max extends Metric { def apply(xs: Seq[Double]) = xs.max }
case object Mean extends Metric { def apply(xs: Seq[Double]) = xs.sum / xs.size }
case object StdDev extends Metric {
  def apply(xs: Seq[Double]) = {
    val mean = xs.sum / xs.size
    val variance = xs.map(x => (x - mean) * (x - mean)).sum
    sqrt(variance)
  }
}

sealed trait Target { def apply(c: ColorScheme): Seq[Double] }
case class Chroma(groupId: Int) extends Target { def apply(c: ColorScheme) = c(groupId).map(_.lch.chroma) }
case class Luminance(groupId: Int) extends Target { def apply(c: ColorScheme) = c(groupId).map(_.lab.luminance) }
case class IntraGroupDistance(groupId: Int) extends Target {
  def apply(c: ColorScheme) = {
    // println("combos: " + c(groupId).combinations(2).toList)
    c(groupId).combinations(2).map { case Seq(a, b) => a distanceTo b }.toSeq
  }
}
case class InterGroupDistance(groupIdA: Int, groupIdB: Int) extends Target {
  def apply(c: ColorScheme) = for (a <- c(groupIdA); b <- c(groupIdB)) yield a distanceTo b
}

sealed trait Modifier { def apply(x: Double): Double }
case class Factor(factor: Double) extends Modifier { def apply(x: Double) = factor * x }
case class Exponent(exponent: Double) extends Modifier { def apply(x: Double) = pow(x, exponent) }
