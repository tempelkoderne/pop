(* PoP Assignment 10g - Animal Race *)
// Tempelkoderne
//      Anders Geil
//      Peter Lim
//      Tobias Stannius

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
    // Additional constructor enabling instances to be created with only one supplied parameter.
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

    // Inherit constructor and additional constructor explicitly.
    new (weight, speedMax) = {inherit Animal (weight, speedMax)}
    new (speedMax) = {inherit Animal (speedMax)}
    // Adjusts the self.Food attribute accordingly.
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
    
    // Inherit constructor and additional constructor explicitly.
    new (weight, speedMax) = { inherit Animal (weight, speedMax) }
    new (speedMax) = { inherit Animal (speedMax) }
    // Adjusts the self.Food attribute accordingly.
    member self.Food = self.weight * 0.40

(*TESTS*)

printfn "" //Terminal eye candy
printfn "             Test: For creating an animal with negative and weight or speed:"
printfn "" //Terminal eye candy

let anorexicVegetarianOtter = Carnivore (-50.0, 30.0)
let anorexicPsychoOtter = Herbivore (-50.0, 30.0)

let leglessVeganGorilla = Herbivore(300.0, -30.0)
let leglessMeatGorilla = Carnivore(300.0, -30.0)

let voidVeganCat = Herbivore(-15.0, -30.0)
let voidMeatCat = Carnivore(-15.0, -30.0)

anorexicVegetarianOtter.Run()
anorexicPsychoOtter.Run()
leglessVeganGorilla.Run()
leglessMeatGorilla.Run()
voidVeganCat.Run()
voidMeatCat.Run()

printfn "anorexicVegetarianOtter =   Carnivore(-50.0, 30.0) = 0.0 speed  :   %b" (anorexicVegetarianOtter.speed = 0.0)
printfn "anorexicPsychoOtter     =   Herbivore(-50.0, 30.0) = 0.0 speed  :   %b" (anorexicPsychoOtter.speed = 0.0)
printfn "leglessVeganGorilla     =   Herbivore (300.0, -30.0) = 0.0 speed:   %b" (leglessVeganGorilla.speed = 0.0)
printfn "leglessMeatGorilla      =   Carnivore (300.0, -30.0) = 0.0 speed:   %b" (leglessMeatGorilla.speed = 0.0)
printfn "voidMeatCat             =   Carnivore (-15.0, -30.0) = 0.0 speed:   %b" (voidVeganCat.speed = 0.0)
printfn "voidVeganCat            =   Herbivore (-15.0, -30.0) = 0.0 speed:   %b" (voidMeatCat.speed = 0.0)

printfn "" //Terminal eye candy
printfn "             Test: Animal race:"
printfn "" //Terminal eye candy

let cheetah = Carnivore (50.0, 114.0)
let antelope = Herbivore (50.0, 95.0)
let wildebeest = Herbivore (200.0, 80.0)

for i = 1 to 3 do
    printfn "Round: %A \n" i
    printfn "--------------------------------------------------------------------------------------------------------------------------------------"
    cheetah.Run()
    wildebeest.Run()
    antelope.Run()
    printfn "Cheetah speed    : %A km/h.   Cheetah food    :%36A   Cheetah food needed    : %A Covering 10km in %A hour(s)" cheetah.speed cheetah.Food (cheetah.FoodNeed()) (10.0/cheetah.speed)
    printfn "Wildebeest speed : %A km/h. Wildebeest food   :%36A  Wildebeest food needed  : %A Covering 10km in %A hour(s)" wildebeest.speed wildebeest.Food (wildebeest.FoodNeed()) (10.0/cheetah.speed)
    printfn "Antelope speed   : %A km/h.  Antelope food    :%36A   Antelope food needed   : %A Covering 10km in %A hour(s)" antelope.speed antelope.Food (antelope.FoodNeed()) (10.0/cheetah.speed)
    printfn ""
    printfn "The Winner of round %A is:" i
    if (cheetah.speed > antelope.speed) && (cheetah.speed > wildebeest.speed) then
        printfn "Winner: Cheetah!"
    elif (antelope.speed > cheetah.speed) && (antelope.speed > wildebeest.speed) then
        printfn "Winner: Antelope!"
    elif (wildebeest.speed > cheetah.speed) && (wildebeest.speed >antelope.speed) then
        printfn "Winner: Wildebeest!"
    else
        printfn "It's a DRAW!!"
    
    printfn "--------------------------------------------------------------------------------------------------------------------------------------"
    printfn ""
