class QuizOne(val x: Int){

    def filterGreater(list: List[Int]): List[Int] = {

        for(number <- list
        if (number > x)
        ) yield number
    }

}
