package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    assert(out.getSignal === true, "and 3")
  }

  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run
    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run
    assert(out.getSignal === true, "or 3")

    in1.setSignal(false)
    run
    assert(out.getSignal === true, "or 4")
  }

  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run
    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run
    assert(out.getSignal === true, "or 3")

    in1.setSignal(false)
    run
    assert(out.getSignal === true, "or 4")
  }

  test("1-to-1 demux example (base case)") {
    val in, out0 = new Wire
    demux(in, List(), List(out0))

    in.setSignal(false)
    run
    assert(out0.getSignal === false)

    in.setSignal(true)
    run
    assert(out0.getSignal === true)
  }

  test("1-to-2 demux example") {
    val in, c0, o1, o0 = new Wire
    demux(in, List(c0), List(o1, o0))

    def assertIO(b: Boolean*) = {
      in.setSignal(b(0))
      c0.setSignal(b(1))
      run
      assert(List(o1, o0).map(_.getSignal) === List(b(2), b(3)))
    }
    //         I        C0       D1     D0
    assertIO(false,   false,   false, false)
    assertIO(false,   true,    false, false)
    assertIO(true,    false,   false, true)
    assertIO(true,    true,    true, false)
  }

  test("1-to-4 demux example") {
    val in, c1, c0, o3, o2, o1, o0 = new Wire
    demux(in, List(c1, c0), List(o3, o2, o1, o0))

    def assertIO(b: Boolean*) = {
      in.setSignal(b(0))
      c1.setSignal(b(1))
      c0.setSignal(b(2))
      run
      assert(List(o3, o2, o1, o0).map(_.getSignal) === List(b(3), b(4), b(5), b(6)))
    }
    //         I        C1    C0        D3     D2     D1    D0
    assertIO(false,   false, false,   false, false, false, false)
    assertIO(false,   false, true,    false, false, false, false)
    assertIO(false,   true, false,    false, false, false, false)
    assertIO(false,   true, true,     false, false, false, false)

    assertIO(true,    false, false,   false, false, false, true)
    assertIO(true,    false, true,    false, false, true, false)
    assertIO(true,    true, false,    false, true, false, false)
    assertIO(true,    true, true,     true, false, false, false)
  }

}
