(*Animal*)
///<summary>
/// A class object.
///</summary>
///<params name="weight">
/// A float.
///</params>
///<params name="speedMax">
/// A float.
///</params>
///<returns>
/// An instance of the Animal object with the assigned weight and speedMax,
/// or a randomly generated weight and assigned speedMax.
///</returns>
///<remarks>
/// An additional constructor allows an instance to be created with only one
/// supplied parameter-value to speedMax.
///</remarks>

type Animal =
    // Constructor parameters.
    val weight : float
    val speedMax : float
    val mutable speed : float
    // Constructor.
    new (weight, speedMax) = {
        weight = max 0.0 weight
        speedMax = max 0.0 speedMax
        speed = 0.0
        }
    // Additional constructor.
    new (speedMax:float) = {
        weight = float (System.Random().Next(70,301))
        speedMax = max 0.0 speedMax
        speed = 0.0
        }
    // Attribute. Generates an amount of Food based on a random percentage of FoodNeed.
    member self.Food = self.FoodNeed() * (float (System.Random().Next(0, 101)))/100.0 
    // Methods for setting FoodNeeded and speed.
    member self.FoodNeed() = self.weight/2.0
    member self.Run() = (self.speed <- self.speedMax * self.Food/100.0)

(*Carnivore*)
///<summary>
/// A class object. Inherits Animal.
///</summary>
///<params name="weight">
/// A float. (Via Animal)
///</params>
///<params name="speedMax">
/// A float. (Via Animal)
///</params>
///<returns>
/// An instance of the Carnivore object with the assigned weight and speedMax,
/// or a randomly generated weight and assigned speedMax.
///</returns>
type Carnivore =
    inherit Animal
    
    new (weight, speedMax) = {inherit Animal (weight, speedMax)}
    new (speedMax) = {inherit Animal (speedMax)}    
    member self.Food = self.weight * 0.08

(*Herbivore*)
///<summary>
/// A class object. Inherits Animal.
///</summary>
///<params name="weight">
/// A float. (Via Animal)
///</params>
///<params name="speedMax">
/// A float. (Via Animal)
///</params>
///<returns>
/// An instance of the Herbivore object with the assigned weight and speedMax,
/// or a randomly generated weight and assigned speedMax.
///</returns>
type Herbivore =
    inherit Animal
    
    new (weight, speedMax) = { inherit Animal (weight, speedMax) }
    new (speedMax) = { inherit Animal (speedMax) }

    member self.Food = self.weight * 0.40


let cheetah = Carnivore (50.0, 114.0)
let antelope = Herbivore (50.0, 95.0)
let wildebeest = Herbivore (200.0, 80.0)

cheetah.Run()
antelope.Run()
wildebeest.Run()

if (cheetah.speed > antelope.speed) && (cheetah.speed > wildebeest.speed) then
    printfn "Winner: Cheetah!"
elif (antelope.speed > cheetah.speed) && (antelope.speed > wildebeest.speed) then
    printfn "Winner: Antelope!"
else
    printfn "Winner: Wildebeest!"


let randomCheetah = Carnivore (50.0)
let randomAntelope = Herbivore (50.0)
let randomWildebeest = Herbivore (200.0)

randomCheetah.Run()
randomAntelope.Run()
randomWildebeest.Run()

if (randomCheetah.speed > randomAntelope.speed) && (randomCheetah.speed > randomWildebeest.speed) then
    printfn "Winner: Cheetah!"
elif (randomAntelope.speed > randomCheetah.speed) && (randomAntelope.speed > randomWildebeest.speed) then
    printfn "Winner: Antelope!"
else
    printfn "Winner: Wildebeest!"
