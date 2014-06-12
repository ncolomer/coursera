package simulations

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal

  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " + wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) {output.setSignal(!inputSig)}
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) {output.setSignal(a1Sig & a2Sig)}
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) {output.setSignal(a1Sig | a2Sig)}
    }
    a1 addAction orAction
    a2 addAction orAction
  }

  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val a1OutSig, a2OutSig, bOutSig = new Wire
    def invAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(InverterDelay) {inverter(a1, a1OutSig); inverter(a2, a2OutSig)}
    }
    a1 addAction invAction
    a2 addAction invAction
    def andAction() {
      afterDelay(AndGateDelay) {andGate(a1OutSig, a2OutSig, bOutSig)}
    }
    a1OutSig addAction andAction
    a2OutSig addAction andAction
    def inv2Action() {
      afterDelay(InverterDelay) {inverter(bOutSig, output)}
    }
    bOutSig addAction inv2Action
  }

  def demux1to2(in: Wire, c: Wire, o1: Wire, o0: Wire) = {
    val ci = new Wire
    inverter(c, ci)
    andGate(in, ci, o0)
    andGate(in, c, o1)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]): Unit = (c, out) match {
    case (Nil, o :: Nil) => andGate(in, in, o)
    case (c :: Nil, o1 :: o0 :: Nil) => demux1to2(in, c, o1, o0)
    case (c :: cs, out) =>
      val o1, o0 = new Wire
      demux1to2(in, c, o1, o0)
      val split = out.splitAt(out.size / 2)
      demux(o1, cs, split._1)
      demux(o0, cs, split._2)
  }

}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
}
