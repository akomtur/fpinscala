package de.kaubisch.fpinscala.chapter6

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by kaubisch on 18.03.16.
  */
class MachineSpec extends FlatSpec with Matchers {

  val machine = Machine(true, 10, 0)

  "Candy Machine" should "have same size of candies when turning the knob without insert a coin" in {
    State.simulateMachine(Turn :: Nil)(machine)._1 shouldBe (10, 0)
  }

  it should "have one more coin if you insert a coin into the machine" in {
    State.simulateMachine(Coin :: Nil)(machine)._1 shouldBe (10, 1)
  }

  it should "not accept more coins if machine is unlocked" in {
    State.simulateMachine(Coin :: Nil)(machine.copy(locked=false))._1 shouldBe (10, 0)
  }

  it should "dispense one candy if the machine is unlocked" in {
    State.simulateMachine(Turn :: Nil)(machine.copy(locked=false))._1 shouldBe (9, 0)
  }

  it should "lock the machine when a candy was released" in {
    State.simulateMachine(Turn :: Nil)(machine.copy(locked=false))._2.locked shouldBe true
  }

  it should "unlock the machine if you insert a coin" in {
    State.simulateMachine(Coin :: Nil)(machine)._2.locked shouldBe false
  }

  it should "have the correct candies and coins after dispensing many candies" in {
    State.simulateMachine(Coin :: Turn :: Coin :: Turn :: Coin :: Turn :: Nil)(machine)._1 shouldBe (7, 3)
  }
}
