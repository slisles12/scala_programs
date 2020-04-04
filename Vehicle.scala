class Vehicle (val speed: Int) {

    def name(): String = {
        "Vehicle"
    }
}

class Car(override val speed) extends Vehicle (speed){
    override def name(): String = "Car"
}

class Bike(override val speed) extends Vehicle (speed){
    override def name(): String = "Bike"
}

object Vehicle {
    def soundOff(vehicleParam: Vehicle){
        println(vehicleParam.speed)
        println(vehicleParam.name())
    }
}

object Main {
    def main(args: Array[String]){
        
    }
}