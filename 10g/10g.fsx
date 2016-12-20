// 10g.0

type Animal (weight:float, speedMax:float) =
    let mutable currentSpeed = 0.0
    let rand = new System.Random()
    
    member self.Food = (float (rand.Next(0,101))) / 100.0
    member self.Weight = weight
    member self.SpeedMax = speedMax
    member self.Speed
        with get() = currentSpeed

    member self.FoodNeed() = weight/2.0
    member self.SetSpeed() = (currentSpeed <- self.SpeedMax * (self.Food * self.FoodNeed()))

    new (speed_max:float) =
        let rand = System.Random()
        let rweight = float (rand.Next(70,301))
        Animal(rweight, speed_max)


type Carnivore (weight:float, speedMax:float) =
    inherit Animal (weight, speedMax)
    member self.Food = self.Weight * 0.08


type Herbivore (weight:float, speedMax:float) =
    inherit Animal (weight, speedMax)
    member self.Food = self.Weight * 0.40


let cheetah = Carnivore (50.0, 114.0)
let antelope = Herbivore (50.0, 95.0)
let wildebeest = Herbivore (200.0, 80.0)

food_actual = random % of self.FoodMax
food_equilibrium() = weight/2
speed() = speedMax * food_actual/food_equilibrium 
