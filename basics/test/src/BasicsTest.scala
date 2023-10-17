// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2022-23 Rex Kerr and Calico Life Sciences, LLC.

package kse.flow.test


import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import scala.collection.generic.IsIterable
import scala.reflect.{ClassTag, TypeTest}
import scala.util.{Try, Success, Failure}
import scala.util.control.ControlThrowable
import scala.util.boundary
import scala.util.boundary.break

import sourcecode.{Line, given}

@RunWith(classOf[JUnit4])
class FlowTest {
  import kse.testutilities.TestUtilities.{_, given}
  import kse.flow.{_, given}

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue _
  )

  @Test
  def testTest(): Unit = {}
}
object FlowTest {
  // @BeforeClass
  // def before(): Unit = { println("Before") }

  // @AfterClass
  // def after(): Unit = { println("After") }
}
