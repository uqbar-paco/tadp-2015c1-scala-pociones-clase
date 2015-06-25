package object pociones {

  type Niveles = (Int, Int, Int)
  type Persona = (String, Niveles)
  type Pocion = (String, List[Ingrediente])
  type Ingrediente = (String, Int, List[Efecto])
  type Efecto = Niveles => Niveles

  import math._

  def aplicar3(f: Int => Int)(caracteristicas: Niveles) =
    (f(caracteristicas._1), f(caracteristicas._2), f(caracteristicas._3))

  //  def invertir3(caracteristicas: Niveles) =
  //    (caracteristicas._3, caracteristicas._2, caracteristicas._1)
  def invertir3(caracteristicas: Niveles) = {
    caracteristicas match {
      case tupla @ (_, _, _) =>
        (tupla._3, tupla._2, tupla._1)
    }
    object x {
      def unapply(bla: Int) =
        Some((bla, bla, bla))
    }
    val x(a, b, c) = 1
    val (ns, nc, nf) = caracteristicas
    (nf, nc, ns)
  }

  val f: Int => Int = {
    case saraza if saraza < 2 => 4
  }

  def suerte(niveles: Niveles) = niveles._1
  def convencimiento(niveles: Niveles) = niveles._2
  def fuerza(niveles: Niveles) = niveles._3

  def nombre(persona: Persona) = persona._1
  def niveles(persona: Persona) = persona._2

  def nombrePocion(pocion: Pocion) = pocion._1
  def ingredientes(pocion: Pocion) = pocion._2

  def nombreIngrediente(ingrediente: Ingrediente) = ingrediente._1
  def cantidad(ingrediente: Ingrediente) = ingrediente._2
  def efectos(ingrediente: Ingrediente) = ingrediente._3

  def sinRepetidos[T](lista: List[T]): List[T] =
    lista match {
      case Nil => Nil
      case x :: xs if xs.contains(x) =>
        sinRepetidos(xs)
      case x :: xs => x :: sinRepetidos(xs)
    }

  def maximo(lista: List[Int]) = lista.max

  def maximoF[T](f: T => Int, lista: List[T]): T =
    lista.maxBy(f)

  val personas = List(("Harry", (11, 5, 4)), ("Ron", (6, 4, 6)), ("Hermione", (8, 12, 2)), ("Draco", (7, 9, 6)))

  val f1: Efecto =
    { case (s, c, f) => (s + 1, c + 2, f + 3) }

  def max2(n: Int)(m: Int) = max(n, m)
  def min2(n: Int)(m: Int) = min(n, m)

  val f2: Efecto =
    aplicar3(_.max(7))

  val f3: Efecto = {
    case (ns, nc, nf) if ns >= 8 => (ns, nc, nf + 5)
    case (ns, nc, nf) => (ns, nc, nf - 3)
  }

  val f5: PartialFunction[Niveles, Niveles] = {
    case (ns, nc, nf) if ns >= 8 => (ns, nc, nf + 5)
    case (ns, nc, nf) => (ns, nc, nf - 3)
  }

  val f4: Efecto = { case (a, b, c) => (a, a, c) }

  val multijugos = ("Multijugos", List(("Cuerno de Bicornio en Polvo", 10, List(invertir3 _, f4)),
    ("Sanguijuela hormonal", 54, List((aplicar3(_ * 2) _), f4))))

  val felixFelices = ("Felix Felices", List(("Escarabajos Machacados", 52, List(f1, f2)), ("Ojo de Tigre Sucio", 2, List(f3))))

  val floresDeBach = ("Flores de Bach", List(("Orquidea Salvaje", 8, List(f3)), ("Rosita", 1, List(f1))))

  def misPociones: List[Pocion] = List(felixFelices, multijugos, floresDeBach)

  // Punto 1
  val sumaNiveles: Niveles => Int = {
    case (s, c, f) => s + c + f
  }

  implicit class FComposition[A, B](f: A => B) {
    def °[C](g: C => A): C => B = f.compose(g)
    def <-|[C](g: C => A): C => B = f.compose(g)
  }

  val diferenciaNiveles: Niveles => Int = {
    case (s, c, f) =>
      //      max(max(s,c), f) - min(min(s,c), f)
      (((_: Int).max(s)) ° ((_: Int).max(c)))(f)
      -(min2(s) _ ° min2(c))(f)

      s.max(c).max(f) - s.min(c).min(f)
  }

  //  def sumaNivelesPersona(persona: Persona) = (sumaNiveles _ ° niveles)(persona)
  val sumaNivelesPersona = sumaNiveles ° niveles

  val diferenciaNivelesPersona = diferenciaNiveles ° niveles

  // Punto 2
  def efectosDePocion(pocion: Pocion) = {
    //    ingredientes(pocion).flatMap(efectos _)
    (ingredientes _ andThen (_ flatMap (efectos _)))(pocion)
  }

  // Punto 3
  val pocionesHeavies =
    ((_: List[Pocion]) filter (efectosDePocion _ andThen (_ size) andThen (_ >= 4))) andThen (_.map(nombrePocion(_)))
  def pocionesHeaviesForExpression(pociones: List[Pocion]) =
    for (pocion @ (nombre, _) <- pociones if efectosDePocion(pocion).size >= 4) yield nombre

  // Punto 4
  def incluyeA(incluida: List[Int], incluyente: List[Int]) = incluida.forall(incluyente.contains(_))

  val vocales = List("a", "e", "i", "o", "u")
  val tieneTodasLasVocales: String => Boolean = { nombre => vocales.forall(nombre.contains(_)) }
  val esPar: Integer => Boolean = _ % 2 == 0
  val algunIngredienteTieneTodasLasVocales: List[Ingrediente] => Boolean = _.exists { ing => tieneTodasLasVocales(ing._1) }
  val todosCantidadesPares: List[Ingrediente] => Boolean = _.forall { ing => esPar(ing._2) }

  val esPocionMagica: Pocion => Boolean = {
    pocion =>
      algunIngredienteTieneTodasLasVocales(pocion._2) && todosCantidadesPares(pocion._2)
  }

  // Punto 5
  def tomarPocion(pocion: Pocion, persona: Persona) = {
    efectosDePocion(pocion).foldLeft(persona) { (persona, efecto) => (nombre(persona), efecto(niveles(persona))) }
  }

  val tomarPocion2: (Pocion, Persona) => Persona =
    efectosDePocion(_).foldLeft(_) {
      case ((nombre, niveles), efecto) =>
        (nombre, efecto(niveles))
    }

  // Punto 6
  val esAntidoto: (Persona, Pocion, Pocion) => Boolean = { (persona, pocion, antidoto) =>
    tomarPocion(antidoto, tomarPocion(pocion, persona)) == persona
  }

  implicit class PocionTomable(pocion: Pocion) {
    def apply(persona: Persona): Persona = tomarPocion(pocion, persona)
  }

  def pp(p: Pocion) = p(_)
  def esAntidoto2(antidoto: Pocion, pocion: Pocion, persona: Persona) = ((persona == _) ° (antidoto(_: Persona)) ° (pocion(_: Persona)))(persona)
  
  //Punto 7
  type Ponderacion = Niveles => Int
  
  val personaMasAfectada: (Pocion, Ponderacion, List[Persona]) => Persona = {(pocion, ponderacion, personas) =>
    maximoF({p: Persona => ponderacion(tomarPocion(pocion, p)._2)}, personas)
  }

  // Punto 8
  val promedioDeNiveles: Niveles => Int = {niveles => sumaNiveles(niveles) / 3}
  val fuerzaFisica: Niveles => Int = {niveles => niveles._3}

}