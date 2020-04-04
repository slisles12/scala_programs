def parity(l: List[boolean]): Boolean = l.foldLeft(false) {_ ^ _}

def main(args: Array[String]): Unit = {
    println(parity)
}