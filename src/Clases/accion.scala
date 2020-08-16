package Clases

case class accion (part:Array[Particulas]){
  def gbest(i:Int =0, Mejor:Particulas = part(0)):Particulas = {
    if (i == part.length-1){
      if (Fit(Mejor) > Fitness(part)(i)){
        return part(i)
      }else{
        return Mejor
      }
    }else{
      if(Fit(Mejor) > Fitness(part)(i)){
        return gbest(i+1, part(i+1));
      }else{
        return gbest(i+1, Mejor)
      }
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
   def Fit(Part:Particulas):Double={
      val f = Funciones(Part.Particula) 
    return f.sphere() // a ver angelica, aqui pones la funcion que quieras(no se te olvide por dios)
  }
}