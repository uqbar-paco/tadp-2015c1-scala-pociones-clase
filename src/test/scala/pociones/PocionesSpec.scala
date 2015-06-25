package pociones

import org.scalatest.FlatSpec
import pociones._
import org.scalatest.Matchers

class PocionesSpec extends FlatSpec with Matchers {

  "sumaNiveles((1,2,3))" should "be 6" in {
    sumaNiveles((1, 2, 3)) shouldBe 6
  }
  
  "diferenciaNiveles((1, 2, 3))" should "be 2" in {
    diferenciaNiveles((1, 2, 3)) shouldBe 2
  }
  
  "sumaNiveles with Harry" should "be 20" in {
    sumaNivelesPersona(("Harry", (11, 5, 4))) shouldBe 20
  }

  "diferenciaNivelesPersona with harry" should "be 7" in {
    diferenciaNivelesPersona(("Harry", (11, 5, 4))) shouldBe 7
  }

  "efectosDePocion felix felices" should "be f1, f2 and f3" in {
    val efectos = efectosDePocion("Felix Felices", List(("Escarabajos Machacados", 5, List(f1, f2)), ("Ojo de Tigre", 2, List(f3))))
    efectos shouldBe List(f1, f2, f3)
  }

  "Multijugos" should "be pocion Heavy" in {
    val pociones = pocionesHeavies(misPociones)
    pociones shouldBe List("Multijugos")
  }

  "Multijugos" should "be pocion Heavy with for expression" in {
    val pociones = pocionesHeaviesForExpression(misPociones)
    pociones shouldBe List("Multijugos")
  }

  "1 to 10" should "include 3,6 and 9" in {
    incluyeA(List(3, 6, 9), (1 until 10).toList) shouldBe true
  }

  "Multijugos" should "be pocionMagica" in {
    esPocionMagica(multijugos) shouldBe true
  }

  "felix Felices" should "Not be magical" in {
    esPocionMagica(felixFelices) shouldBe false
  }

  "floresDeBach" should "not be magical" in {
    esPocionMagica(floresDeBach) shouldBe false
  }

  "tomarPocion Harry" should "change state" in {
    tomarPocion(felixFelices, ("Harry", (11, 5, 4))) shouldBe ("Harry", (12, 7, 12))
  }

  "personaMasAfectada by multijugos with sumaNiveles" should "be Draco" in {
    val persona = personaMasAfectada(multijugos, sumaNiveles, personas)
    persona shouldBe ("Harry", (11, 5, 4))
  }

  "personaMasAfectada by multijugos with promedioDeNiveles" should "be Draco" in {
    val persona = personaMasAfectada(multijugos, promedioDeNiveles, personas)
    persona shouldBe ("Harry", (11, 5, 4))
  }

  "personaMasAfectada by multijugos with fuerzaFisica" should "be Harry" in {
    val persona = personaMasAfectada(multijugos, fuerzaFisica, personas)
    persona shouldBe ("Harry", (11, 5, 4))
  }
  
  "personaMasAfectada by multijugos with diferenciaNiveles" should "be Harry" in {
    val persona = personaMasAfectada(multijugos, diferenciaNiveles, personas)
    persona shouldBe ("Harry", (11, 5, 4))
  }
  
}