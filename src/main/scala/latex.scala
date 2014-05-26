package latex

object latex {
    def wrap(statement: String) =
        "\\" + statement

    def unit(number: Any, u: String) = 
        "\\unit[" + number.toString + "]{" + u + "}"

    def roundAt(p: Int, n: Double) = {
        val s = math.pow (10, p); 
        (math.round(n * s)) / s
    }
}

