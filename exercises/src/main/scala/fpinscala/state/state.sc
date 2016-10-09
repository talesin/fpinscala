import fpinscala.state._

object state {
  case class TestRNG(x: Int) extends RNG {
    override def nextInt: (Int, RNG) = (x, new TestRNG(x+1))
  }

  val rng = TestRNG(1)

  RNG.nonNegativeInt(TestRNG(1))
  RNG.nonNegativeInt(TestRNG(-1))
  RNG.nonNegativeInt(TestRNG(Int.MinValue))
  RNG.nonNegativeInt(RNG.Simple(99L))

  RNG.double(TestRNG(1))
  RNG.double(TestRNG(Int.MaxValue))
  RNG.nonNegativeInt(RNG.Simple(99L))

  RNG.double(TestRNG(1))
  RNG.double(TestRNG(Int.MaxValue))
  RNG.double(RNG.Simple(99L))

  RNG.doubleInt(TestRNG(1))
  RNG.doubleInt(TestRNG(Int.MaxValue))
  RNG.doubleInt(RNG.Simple(99L))

  RNG.intDouble(TestRNG(1))
  RNG.intDouble(TestRNG(Int.MaxValue))
  RNG.intDouble(RNG.Simple(99L))

  RNG.double3(TestRNG(1))
  RNG.double3(TestRNG(Int.MaxValue))
  RNG.double3(RNG.Simple(99L))

  RNG.ints(5)(TestRNG(1))
  RNG.ints(5)(RNG.Simple(99))

  RNG.ints$(5)(TestRNG(1))
  RNG.ints$(5)(RNG.Simple(99))

  RNG.ints$$(5)(TestRNG(1))
  RNG.ints$$(5)(RNG.Simple(99))

  RNG.map(RNG.int)(identity)(TestRNG(1))
  RNG.map(RNG.int)(identity)(RNG.Simple(1))

  RNG.map$(RNG.int)(identity)(TestRNG(1))
  RNG.map$(RNG.int)(identity)(RNG.Simple(1))

  RNG.map2(RNG.int,RNG.int)((a,b) => (a,b))(TestRNG(1))
  RNG.map2(RNG.int,RNG.int)((a,b) => (a,b))(RNG.Simple(1))

  RNG.map2$(RNG.int,RNG.int)((a,b) => (a,b))(TestRNG(1))
  RNG.map2$(RNG.int,RNG.int)((a,b) => (a,b))(RNG.Simple(1))

  RNG.rollDie(TestRNG(1))
  RNG.rollDie(RNG.Simple(1))


  Machine.simulateMachine(List(Coin, Turn)).run(Machine(locked = true, 10, 0))
  Machine.simulateMachine(List(Turn, Coin, Coin, Turn)).run(Machine(locked = true, 10, 0))
  Machine.simulateMachine(List(Turn, Coin, Coin, Turn)).run(Machine(locked = true, 0, 1))
  Machine.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn)).run(Machine(locked = true, 10, 0))
}