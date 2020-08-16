package Clases

case class Funciones(Particulas:Array[Double]) {
  def sphere(Posicion:Int = Particulas.length-1, Valor:Double = Particulas(Particulas.length-1)) :Double={
    if(Posicion == 0){
      return(this.Particulas(0)*this.Particulas(0))
    }else{
      return (Valor*Valor)+sphere(Posicion-1, this.Particulas(Posicion-1))
    }
  }
  def rosenbrock(Posicion:Int = 0) :Double={
    if(Posicion == ((Particulas.length)/2)-2){
      return(100.0*(((Particulas(2*Posicion)*Particulas(2*Posicion))- Particulas((2*Posicion)+1))+((Particulas(2*Posicion)*Particulas(2*Posicion))-1)*(Particulas(2*Posicion)*Particulas(2*Posicion))-1))
    }else{
      return (100.0*(((Particulas(2*Posicion)*Particulas(2*Posicion))- Particulas((2*Posicion)+1))+((Particulas(2*Posicion)*Particulas(2*Posicion))-1)*(Particulas(2*Posicion)*Particulas(2*Posicion))))+rosenbrock(Posicion+1)
    }
  }
}