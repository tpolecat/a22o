// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package allocation
package parser

import cats.implicits._
import com.google.monitoring.runtime.instrumentation.AllocationRecorder
import com.google.monitoring.runtime.instrumentation.Sampler
import a22o.Parser
import munit.FunSuite

trait AllocationSuite extends FunSuite {

  implicit class ParserOps[A](pa: Parser[A]) {

    def assertNoAllocation(successCase: String, failureCase: String): Unit = {
      parseWithAllocationAssertions(pa, successCase, true)()
      parseWithAllocationAssertions(pa, failureCase, false)()
      parseWithAllocationAssertions(pa.void, successCase, true)()
      parseWithAllocationAssertions(pa.void, failureCase, false)()
    }

    def assertNoAllocation0(successCase: String): Unit = {
      parseWithAllocationAssertions(pa, successCase, true)()
      parseWithAllocationAssertions(pa.void, successCase, true)()
    }

    def assertNoAllocation1(failureCase: String): Unit = {
      parseWithAllocationAssertions(pa, failureCase, false)()
      parseWithAllocationAssertions(pa.void, failureCase, false)()
    }

    def assertAllocation(successCase: String, failureCase: String)(alloc: (String, Int)*): Unit = {
      parseWithAllocationAssertions(pa, successCase, true)(alloc: _*)
      parseWithAllocationAssertions(pa, failureCase, false)()
      parseWithAllocationAssertions(pa.void, successCase, true)()
      parseWithAllocationAssertions(pa.void, failureCase, false)()
    }

    def assertAllocation0(successCase: String)(alloc: (String, Int)*): Unit = {
      parseWithAllocationAssertions(pa, successCase, true)(alloc: _*)
      parseWithAllocationAssertions(pa.void, successCase, true)()
    }

  }

  private def parseWithAllocationAssertions(pa: Parser[_], s: String, success: Boolean)(alloc: (String, Int)*): Unit = {
    val expected = alloc.toList.foldMap { case (k, v) => Map(k -> v) }
    var map: Map[String, Int] =
      Map(
        // these are always allocated
        "a22o/MutState" -> -1,
        "com/google/monitoring/runtime/instrumentation/Sampler" -> -1,
      )
    val sampler = new Sampler {
      def sampleAllocation(count: Int, desc: String, newObj: Object, size: Long): Unit = {
        //println(desc)
        map |+|= Map(desc -> 1)
      }
    }
    pa.parse(s) // get classloading out of the way
    AllocationRecorder.addSampler(sampler)
    val ok = pa.accept(s)
    AllocationRecorder.removeSampler(sampler)
    assertEquals(ok, success)
    assertEquals(map.filter { case (_, v) => v > 0 }, expected)
  }

}
