package Clases
import java.util.Random
object main {
  
  def main(arg: Array[String]) {
    
    val random = new Random()
    // Particulas, Dimensiones, iteraciones
    val P =8 // cantidad de particulas
    val D =20 // cantidad de dimensiones
    val I =5 // cantidad de iteraciones
    val W= 0.5 // se supone que esto es fi (el documento pide que sea un numero entre 0 y 1.2)
    val c1 = 1  // se supone que esto es fi1 (el documento pide que sea un numero entre 0 y 2)
    val c2 = 1  // se supone que esto es fi2 (el documento pide que sea un numero entre 0 y 2)
    val Particulas =new Array[Particulas](P)
    val V =new  Array[Particulas] (P)
    for (i <- 0 until P) {
      val particula =new  Array[Double](D)
      val v =new  Array[Double](D)
    for (j<- 0 until D) {
      //println(j)
      particula(j) = random.nextDouble()*10-5
      v(j) = random.nextDouble()*2-1
    }
    Particulas(i) = new Particulas(particula)
    V(i) = new Particulas(v)
    //println()
    }
    val pbest = Particulas
    for(j<- 0 until I){
    val fitness = Fitness(Particulas)
    val acc = new accion(Particulas)
    val gbest = acc.gbest()
    Pbest(Particulas, pbest)
    ActualizarV(V, Particulas, W, c1, c2, random, gbest, pbest)
    ActualizarX(V, Particulas)
    ver(Particulas)
    }
  }
  def Fitness(Particulas:Array[Particulas]):Array[Double]={
    val fitness =new Array[Double](Particulas.length)
    for (i <- 0 until Particulas.length) {
      val f = Funciones(Particulas(i).Particula) 
      fitness(i) = f.sphere() // a ver angelica, aqui pones la funcion que quieras(no se te olvide por dios)
    }
    return fitness
  }
   def Pbest(particulas:Array[Particulas],gbest:Array[Particulas]){
    val mejor =new  Array[Particulas](particulas.length)
    for (i <- 0 until particulas.length) {
    if(Fitness(particulas)(i) < Fitness(gbest)(i)){
      gbest(i) = particulas(i)
    }  
    }
  }
   def ActualizarV(V:Array[Particulas],particulas:Array[Particulas], W:Double, C1:Double, C2:Double, r:Random, gbest:Particulas, pbest:Array[Particulas]){
     for (i <- 0 until particulas.length) {
     for (j <- 0 until particulas(i).Particula.length) {
       V(i).Particula(j) = W* V(i).Particula(j)+(C1*r.nextDouble()*(pbest(i).Particula(j)-particulas(i).Particula(j)))+(C2*r.nextDouble()*(gbest.Particula(j)-particulas(i).Particula(j)))
     }  
     }
   }
   def ActualizarX(V:Array[Particulas],particulas:Array[Particulas]){
     for (i <- 0 until particulas.length) {
     for (j <- 0 until particulas(i).Particula.length) {
       particulas(i).Particula(j) = particulas(i).Particula(j) + V(i).Particula(j)
        }  
     }
   }
   def ver(V:Array[Particulas]){
     for (i <- 0 until V.length) {
     for (j <- 0 until V(i).Particula.length) {
       printf(" %f ",V(i).Particula(j))
     }
     println()
     }
     }
}