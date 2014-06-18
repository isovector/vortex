package latex

import spire.math._
import spire.implicits._

object latex {
    val Sz = new Matrix(List(mkBra(1, 0), mkBra(0, -1)))
    val Sx = new Matrix(List(mkBra(0, 1), mkBra(1, 0)))
    val Sy = new Matrix(List(mkBra(0, Complex(0, -1)), mkBra(Complex(0, 1), 0)))

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

        val output = result.replaceAll("0+$", "").replaceAll("\\.$", "")
        if (output == "")
            "0"
        else
            output
    }

    object mkBra {
        def apply(varDims: Complex[Double]*) = {
            new Bra(varDims.toList)
        }
    }

    case class Basis(varNames: String*) {
        val names = varNames.toList
    }

    case class Bra(dims: Seq[Complex[Double]]) {
        def plus(that: Bra) = {
            new Bra((dims, that.dims).zipped map (_ + _))
        }

        def -(that: Bra) = {
            new Bra((dims, that.dims).zipped map (_ - _))
        }

        def conjugate = {
            new Bra(dims map (_.conjugate))
        }

        def times(that: Ket): Complex[Double] = {
            (dims, that.dims).zipped.map(_ * _).fold(new Complex[Double](0, 0))(_ + _)
        }

        def times(scalar: Complex[Double]) = {
            new Bra(dims map (_ * scalar))
        }

        def toKet = {
          new Ket(conjugate.dims)
        }

        def asLatex = {
            "\\begin{bmatrix}" +
            dims.map { dim =>
                clean(dim)        
            }.mkString("&") +
            "\\end{bmatrix}"
        }
    }

    object mkKet {
        def apply(varDims: Complex[Double]*) = {
            new Ket(varDims.toList)
        }
    }

    case class Ket(dims: Seq[Complex[Double]]) {
        def plus(that: Ket) = {
            new Ket((dims, that.dims).zipped map (_ + _))
        }

        def -(that: Ket) = {
            new Ket((dims, that.dims).zipped map (_ - _))
        }

        def conjugate = {
            new Ket(dims map (_.conjugate))
        }

        def times(that: Bra) = {
            new Matrix(dims.map { a =>
                new Bra(that.dims.map { b =>
                    a * b        
                })
            })
        }

        def times(scalar: Complex[Double]) = {
            new Ket(dims map (_ * scalar))
        }
        def toBra = {
          new Bra(conjugate.dims)
        }

        def outerProduct: Matrix = {
          this.times(toBra)
        }

        def innerProduct: Complex[Double] = {
          toBra.times(this)
        }
        
        def asLatex = {
            "\\begin{bmatrix}" +
            dims.map { dim =>
                clean(dim)        
            }.mkString("\\\\") +
            "\\end{bmatrix}"
        }

        def asLinLatex(dir: String = "")(implicit basis: Basis) = {
            (dims, basis.names).zipped.map { case (dim, name) =>
                "(" + clean(dim) + ")\\Ket{" + name + "}" + 
                (if (dir != "") "_{" + dir + "}")
            }.mkString("+")
        }
    }

    case class Matrix(dims: Seq[Bra]) {
        def plus(that: Matrix) = {
            new Matrix((dims, that.dims).zipped map (_ plus _))
        }

        def times(that: Ket) = {
            new Ket(dims.map { dim =>
                dim times that;
            })
        }

        def times(scalar: Complex[Double]) = {
            new Matrix(dims map (_ times scalar))
        }

        def times(that: Matrix) = {
            new Matrix(dims.map { bra => 
                new Bra(
                    that.columns.map { ket =>
                        bra times ket;
                    }
                )
            })
        }

        def columns = {
            (0 to (dims(0).dims.length - 1)).map { i =>
                new Ket(dims.map { bra =>
                    bra.dims(i)
                })
            }
        }

        def asLatex = {
            "\\begin{bmatrix}" +
            dims.map { bra =>
                bra.dims.map { dim =>
                    clean(dim)
                }.mkString("&")
            }.mkString("\\\\") +
            "\\end{bmatrix}"
        }

        def trace = {
            var i = -1
            dims.foldLeft(Complex[Double](0, 0)) { (a, bra) =>
                i = i + 1
                a + bra.dims(i)
            }
        }
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

