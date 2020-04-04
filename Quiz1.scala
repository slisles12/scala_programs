class QuizOne(val x: Int){

    def filterGreater(val list: List[Int]): List[Int] = {

        for(number <- list
        if (number > x)
        ) yield number
    }

}

