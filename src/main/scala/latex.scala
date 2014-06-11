package latex

import spire.math._
import spire.implicits._

object latex {
    var p1: String = ""
    var p2: String = ""
    var p3: String = ""
    var p4: String = ""
    var p5: String = ""

    def wrap(statement: String) =
        "\\" + statement

    def unit(number: Any, u: String) = 
        "\\unit[" + number.toString + "]{" + u + "}"

    def roundAt(p: Int, n: Double) = {
        val s = math.pow (10, p); 
        (math.round(n * s)) / s
    }

    def clean(x: Any): String = {
        val result = x match {
          case x: Int => "%d".format(x)
          case x: Long => "%d".format(x)

          case x: Float => "%.3f".format(x)
          case x: Double => "%.3f".format(x)
          case x: Complex[Double] => complex(x)
          case x =>  x.toString
        }

        result.replaceAll("0+$", "").replaceAll("\\.$", "")
    }

    def expectation(
            up: Complex[Double],
            down: Complex[Double],
            a: Complex[Double], 
            b: Complex[Double], 
            c: Complex[Double], 
            d: Complex[Double]): Double = {
        (up.conjugate * (a * up + b * down) + 
          down.conjugate * (c * up + d * down)
        ).real
    }

    val epsilon = 0.001
    def complex(c: Complex[Double]): String = {
      val output = c match {
        case Complex(real, imag) if real.abs < epsilon && imag.abs < epsilon 
            => 0
        case Complex(real, imag) if imag.abs < epsilon 
            => clean(real)
        case Complex(real, imag) if real.abs < epsilon 
            => clean(imag) + "i"
        case Complex(real, imag) => 
            clean(real) + 
                (if (imag > 0) "+"
                else { "" }) +
                clean(imag) + "i"
                
      }

      if (output == "1i")
        "i"
      else if (output == "-1i")
        "-i"
      else
        output toString
    }

    case class RichComplex(underlying: Complex[Double]) {
        def conj2: Double = (underlying * underlying.conjugate).real

    }

    implicit def complex2Rich(underlying: Complex[Double]): RichComplex =
        new RichComplex(underlying)
}

