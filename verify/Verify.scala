//> using jvm 25
//> using scala 3.8.3
//> using dep com.github.ichoran::kse3-basics:0.7.1
//> using dep com.github.ichoran::kse3-flow:0.7.1
//> using dep com.github.ichoran::kse3-maths:0.7.1
//> using dep com.github.ichoran::kse3-loom:0.7.1
//> using dep com.github.ichoran::kse3-thyme:0.7.1
//> using dep com.github.ichoran::kse3-eio:0.7.1
//> using dep com.github.ichoran::kse3-jsaun:0.7.1

// Usage: scala-cli run Verify.scala
// Checks to make sure that the latest version is successfully published.

@main
def testPublish(): Unit = {
  import kse.basics.*
  import kse.flow.*
  import kse.maths.*
  import kse.loom.*
  import kse.thyme.*
  import kse.eio.*
  import kse.jsaun.*
  
  def number = (1.0/(1 + Pcg64() % 10)).entropy     // maths
  val elapsed = Thyme().timePair(number)._2.elapsed // thyme
  Fu:                                               // loom
    val digs = number.toString.select(_.isDigit)    // basics
    println(digs.bytes.stringEncode64)              // eio
    Err ?# "Not an error, we're just bored"         // flow
    println(say"You should not see $elapsed.")      // new in 0.7.1
  .await() __ Unit
}
