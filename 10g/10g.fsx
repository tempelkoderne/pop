// 10g.0

type Animal =
    val weight : float
    val speedMax : float
    val mutable speed : float
    new (weight, speedMax) = {
        weight = weight
        speedMax = speedMax
        speed = 0.0
        }
    new (speedMax:float) = {
        weight = float (System.Random().Next(70,301))
        speedMax = speedMax
        speed = 0.0
        }
    
    member self.Food = self.FoodNeed() * (float (System.Random().Next(0, 101)))/100.0 

    member self.FoodNeed() = self.weight/2.0
    member self.Run() = (self.speed <- self.speedMax * self.Food/100.0)


type Carnivore =
    inherit Animal
    
    new (weight, speedMax) = { inherit Animal (weight, speedMax) }
    new (speedMax) = { inherit Animal (speedMax) }    
    member self.Food = self.weight * 0.08


type Herbivore =
    inherit Animal
    
    new (weight, speedMax) = { inherit Animal (weight, speedMax) }
    new (speedMax) = { inherit Animal (speedMax) }

    member self.Food = self.weight * 0.40


let cheetah = Carnivore (50.0, 114.0)
let antelope = Herbivore (50.0, 95.0)
let wildebeest = Herbivore (200.0, 80.0)

