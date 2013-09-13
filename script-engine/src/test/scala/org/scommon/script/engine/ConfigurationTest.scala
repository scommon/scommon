package org.scommon.script.engine

import org.scalatest.junit.JUnitRunner
import org.scalatest.{SeveredStackTraces, FunSuite}
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scommon.script.engine.core.Engine
import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigException.ValidationFailed


@RunWith(classOf[JUnitRunner])
class ConfigurationTest extends FunSuite
with ShouldMatchers
with SeveredStackTraces {
  import core.Utils._

  val VALID_ENGINE_FACTORY = ScalaEngine.typeSymbol.fullName

  test("Empty configuration throws error") {
    intercept[ValidationFailed] {
      Engine.factoriesFromConfiguration(configuration = ConfigFactory.parseString(
        """
          |
        """.stripMargin)
      )
    }
  }

  test("Incorrectly named key throws error") {
    intercept[ValidationFailed] {
      Engine.factoriesFromConfiguration(configuration = ConfigFactory.parseString(
        """
          |script.engine {
          |}
        """.stripMargin)
      )
    }
  }

  test("Missing contents errors") {
    intercept[ValidationFailed] {
      Engine.factoriesFromConfiguration(configuration = ConfigFactory.parseString(
        """
          |script-engine {
          |
          |}
        """.stripMargin)
      )
    }
  }

  test("Missing engine errors") {
    intercept[ValidationFailed] {
      Engine.factoriesFromConfiguration(configuration = ConfigFactory.parseString(
        """
          |script-engine {
          |  default = foo
          |}
        """.stripMargin)
      )
    }
  }

  test("Missing default errors") {
    intercept[ValidationFailed] {
      Engine.factoriesFromConfiguration(configuration = ConfigFactory.parseString(
        """
          |script-engine {
          |  engines = [{
          |    name    = foo
          |    factory = foo
          |  }]
          |}
        """.stripMargin)
      )
    }
  }

  test("Engine not on class path cannot be instantiated") {
    intercept[IllegalStateException] {
      Engine.factoriesFromConfiguration(configuration = ConfigFactory.parseString(
        """
          |script-engine {
          |  default = foo
          |  engines = [{
          |    name    = foo
          |    factory = foo
          |  }]
          |}
        """.stripMargin)
      )
    }
  }

  test("No matching engine as specified by the default key") {
    intercept[IllegalArgumentException] {
      Engine.factoriesFromConfiguration(configuration = ConfigFactory.parseString(
        s"""
          |script-engine {
          |  default = foo
          |  engines = [{
          |    name    = bar
          |    factory = $VALID_ENGINE_FACTORY
          |  }]
          |}
        """.stripMargin)
      )
    }
  }

  test("Valid configuration yields valid list of factories") {
    val from_config = Engine.factoriesFromConfiguration(configuration = ConfigFactory.parseString(
      s"""
        |script-engine {
        |  default = scala-engine
        |  engines = [{
        |    name    = scala-engine
        |    factory = $VALID_ENGINE_FACTORY
        |  }]
        |}
      """.stripMargin)
    )
    from_config.default should be(ScalaEngine)
    from_config.factories should be(Seq(ScalaEngine))
  }

}
