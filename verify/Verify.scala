//> using jvm 21
//> using scala 3.7.2
//> using dep com.github.ichoran::kse3-basics:0.4.0
//> using dep com.github.ichoran::kse3-flow:0.4.0
//> using dep com.github.ichoran::kse3-maths:0.4.0
//> using dep com.github.ichoran::kse3-loom:0.4.0
//> using dep com.github.ichoran::kse3-eio:0.4.0

// Usage: scala-cli run Verify.scala
// Checks to make sure that the latest version is successfully published.

@main
def testPublish(): Unit = {
  import kse.basics.*
  import kse.flow.*
  import kse.maths.*
  import kse.loom.*
  import kse.eio.*
  
  val number = (1.0/(1 + Pcg64() % 10)).entropy  // maths
  Fu:                                            // loom
    val digs = number.toString.select(_.isDigit) // basics
    println(digs.bytes.stringEncode64)           // eio
    Err ?# "Not an error, we're just bored"      // flow
    println("You should not see this message.")
  .ask() __ Unit
}
