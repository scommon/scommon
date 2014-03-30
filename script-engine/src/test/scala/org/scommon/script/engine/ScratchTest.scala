package org.scommon.script.engine

import _root_.scala.Some
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, BeforeAndAfterAll, SeveredStackTraces, FunSuite}
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import java.util.UUID

import _root_.scala.util._

import org.scommon.io._
import Utils._
import _root_.scala.tools.nsc.{SubComponent, Phase, Settings, Global}
import _root_.scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import _root_.scala.tools.nsc.io.VirtualDirectory
import _root_.scala.tools.nsc.io.AbstractFile
import java.net.{URLClassLoader, URI}
import java.io._
import _root_.scala.reflect.io.{Streamable, NoAbstractFile}
import _root_.scala.collection._
import _root_.scala.tools.util.PathResolver
import java.nio.file.{Paths, LinkOption}
import java.nio.charset.{Charset, CharsetDecoder}
import java.nio.ByteBuffer
import _root_.scala.tools.nsc.plugins.{Plugin, PluginComponent}
import _root_.scala.tools.nsc

import org.scommon.core._
import org.scommon.reactive._
import org.scommon.security._
import org.scommon.reflect.Mirror
import org.scommon.security.Sandbox
import org.scommon.script.engine.core._

import rx.lang.scala.Observer


@RunWith(classOf[JUnitRunner])
class ScratchTest extends FunSuite
                     with Matchers
                     with SeveredStackTraces
                     with BeforeAndAfterAll {

  val PARENT_WORKING_DIR = Path(s"scratch-test").toUserTemp

  override protected def beforeAll() = {
    PARENT_WORKING_DIR.mkdirs()
  }

  override protected def afterAll() = {
    PARENT_WORKING_DIR.deleteAll
  }

  def privileged(fn: => Unit): Unit = {
    org.scommon.security.SecurityManager.privilegedWithoutContext(fn)
  }

  def privilegedWithSecurityManager(fn: => Unit): Unit = {
    //This works because we will have created an AccessControlContext before a
    //SecurityManager is set and are then resetting it within the same doPrivileged().
    org.scommon.security.SecurityManager.privilegedWithoutContext({
      val old = org.scommon.security.SecurityManager.installDefault()
      try {
        fn
      } finally {
        org.scommon.security.SecurityManager.install(old)
      }
    })
  }

  test("Scratch") {
    val working = Path(PARENT_WORKING_DIR, s"${UUID.randomUUID().toString}")
    working.mkdirs()

    privilegedWithSecurityManager {
      try {
        val workingDirectory: String = "."
        val customClassPath: Iterable[String] = Seq("a.jar", "b.jar", "c.jar")
        val working_directory: java.nio.file.Path = java.nio.file.Paths.get(workingDirectory).toRealPath(LinkOption.NOFOLLOW_LINKS).toAbsolutePath

        val my = ScalaCompilerSettings()
        //my.inMemory = false
        //my.outputDirectory = Paths.get(working.getAbsolutePath, s"${UUID.randomUUID().toString}")
        //my.outputDirectory.toFile.mkdirs()
        //println(my.outputDirectory)

        my.handlers.progressUpdate = (_, progress) => {
          //println(s"Progress: $progress")
        }

        my.handlers.compileCompleted = (_, result) => {
          //Execute all found main methods
          for (main <- result.discoverMainMethods()) {
            //println(cls)
            Sandbox.run {
              main(Array[String]())
            }
          }


  //        result.unitRunInSandbox(_.context.setDebug(false)) { data =>
  //          import org.scommon.core.ThreadUtil._
  //          val thread = new Thread(new Runnable {
  //            def run(): Unit = {
  //              //Load entry points and execute them.
  //
  //              val runtime_mirror = universe.runtimeMirror(data.classLoader)
  //
  //              for {
  //                entry_point <- data.entryPoints
  //                //cls = Class.forName(entry_point.javaClassName, false, Thread.currentThread().getContextClassLoader())
  //                module    = runtime_mirror.staticModule(entry_point.scalaClassName)
  //                obj       = runtime_mirror.reflectModule(module)
  //                reflected = runtime_mirror.reflect(obj.instance)
  //                method    = obj.symbol.typeSignature.member(newTermName("main")).asMethod
  //                main      = reflected.reflectMethod(method)
  //              } {
  //                //println(cls)
  //                main(Array[String]())
  //              }
  //            }
  //          })
  //
  //          thread.start()
  //          thread.join()
  //
  //        }
        }

        my.customClassPath = customClassPath
        my.relativeDirectory = working_directory
        my.withTypeFilter[MyTestTrait]

        //my.options = Seq("-Xprint-types", "-Xshow-phases", "-Ydebug")

        val engine = Engine.newEngine[Scala](my)

        engine.toObservable(Observer {
          //Successfully compiled
          case CompileCompleted(result) =>
            //println(s">>>>>>>>>> Discovered: ${result.discovered[MyTestTrait]}")
            for {
              d <- result.discovered[MyTestTrait]
              instance <- Mirror.newInstanceWithParameterLists[MyTestTrait](d, result.toClassLoader())(Seq("ABC"), Seq(200))
            } {
              println(s"INSTANCE!!! $instance ${instance.getClass}")
            }

          //Progress update has been received
          case CompileProgressUpdate(progress) =>
            //println(s"$progress")

          //Error during compilation
          case CompileMessageReceived(msg) =>
            //println(s"Failed compile: $msg")
          case CompileFatalError(error) =>
            //println(s"Fatal error: $error")
          case _ =>
        })

        engine.pushSource(EmbeddedResources(1 to 2)("/examples/t001/%03d.scala"))

        usingUnit(engine) {

          val separate_context = CompileContext()
          separate_context.handlers.messageReceived = (e, m) => {
            println(s"OTHER CONTEXT: ${m.message}")
          }

          //Pushing with a different context invokes its handlers.
          engine.pushSource(separate_context, EmbeddedResource("/examples/t002/001.scala"))
        }
      } finally {
        working.deleteAll should be (true)
        working.exists should be (false)
      }
    }
  }
}

trait MyTestTrait

/*
[22:37:07] <TheChistoso> xeno_by: hi! :D did you see my question above?
[22:37:20] <xeno_by> hi yes let me see
[22:37:53] <xeno_by> well for one a ClassSymbol can't represent an object
[22:38:06] Apocalisp [~textual@c-174-62-237-65.hsd1.ma.comcast.net] has quit IRC: Quit: Computer has gone to sleep.
[22:38:10] <xeno_by> secondly, you need to know whether the class is inner or not
[22:38:20] <xeno_by> if it is, then you need an outer reference
[22:38:36] mary5030 [~mary5030@107-193-220-128.lightspeed.austtx.sbcglobal.net] has quit IRC: Remote host closed the connection
[22:38:55] <xeno_by> finally you do the following
[22:39:24] <TheChistoso> okay so what i actually have is a string representing a type along w/ some arguments
[22:39:38] <xeno_by> <some mirror>.reflectClass(<your symbol>).reflectConstructor(<a symbol of class's constructor>)(<arguments>)
[22:39:47] <xeno_by> like here
[22:39:48] <xeno_by> https://github.com/xeno-by/scala/blob/ticket/6411/test/files/run/t6411b.scala#L7
[22:40:22] <xeno_by> well then first you'll have to obtain the ClassSymbol using <some mirror>.staticClass(<your string>)
[22:40:49] <xeno_by> if it's a module, you should do <some mirror>.reflectModule(<your symbol>).instance
[22:41:02] <xeno_by> where <your symbol> will come from <some mirror>.staticModule(<your string>)
[22:41:30] <TheChistoso> what i have is a string representing a type where i know that type mixes in a certain, known trait
[22:42:36] <TheChistoso> so i could have object Foo extends MyTrait or case class Foo(args) extends MyTrait or case object Foo extends MyTrait or class Foo(args) extends MyTrait
[22:42:57] Otherwise [~else@89.22.164.48] has joined #scala
[22:43:06] aiyerk [~aiyerk@c-24-6-222-26.hsd1.ca.comcast.net] has quit IRC: Remote host closed the connection
[22:43:36] edwardk [~edwardk@pdpc/supporter/professional/edwardk] has quit IRC: Quit: Computer has gone to sleep.
[22:43:48] <TheChistoso> xeno_by: here's the signature: def instantiate[T](description: ClassDescription, classLoader: ClassLoader)(args: Any*): Option[T] where "ClassDescription" is my own class that encapsulates a full class name as a string
[22:43:51] edwardk [~edwardk@pdpc/supporter/professional/edwardk] has joined #scala
[22:44:10] <xeno_by> <some mirror> is the missing element i guess
[22:44:22] <TheChistoso> val runtimeMirror = universe.runtimeMirror(classLoader)
[22:44:27] <xeno_by> it can be obtained via scala.reflect.runtime.universe.currentMirror(classLoader)
[22:44:27] <xeno_by> yes
[22:44:49] <TheChistoso> i need to match a constructor given the args (Any*)
[22:45:27] <TheChistoso> and i need to be able to differentiate b/t a module vs. a class that might have constructor args
[22:46:22] <xeno_by> unfortunately selecting an overload from a list of constructors is something where you're on your own
[22:46:36] <xeno_by> this isn't yet implemented in scala-reflect.jar
[22:46:49] <xeno_by> it's a part of the typechecker that we yet have to factor out
[22:47:01] <xeno_by> choosing between a class and a module is going to be much simpler
[22:47:02] <TheChistoso> i can get the list of constructors fine. but it seems this might be complicated in the case of a case class
[22:47:11] <xeno_by> why?
[22:47:28] <TheChistoso> wouldn't i effectively want to search a companion object's apply methods?
[22:47:43] <xeno_by> not necessarily
[22:47:53] <xeno_by> a regular constructor is created as well
[22:48:10] <TheChistoso> ah -- that's good to know :D
[22:48:25] <xeno_by> so if you're only interested in a primary constructor, then everything's fine
[22:48:31] <xeno_by> and you don't need overload resolution
[22:48:48] wilmoore [~wilmoore@c-67-190-17-108.hsd1.co.comcast.net] has quit IRC: Ping timeout: 248 seconds
[22:48:48] <TheChistoso> really? why's that?
[22:49:14] <xeno_by> well, if it's a primary constructor, then it's by definition the only one :)
[22:50:04] <TheChistoso> lol yes of course -- brain fart :D i'm provided an Any* so what i'm given may or may not be arguments for the primary constructor
[22:50:37] mengu [~mengu@unaffiliated/mengu] has joined #scala
[22:50:49] <TheChistoso> i would prefer to check the primary constructor first and then fall back on looking through auxiliary constructors looking for the closest match
[22:52:09] <xeno_by> yeah well that might be problematic
[22:52:22] <xeno_by> because you would have to do overload resolution on your own then
[22:52:27] <xeno_by> overload resolution and type inference*
[22:52:47] wilmoore [~wilmoore@c-67-190-17-108.hsd1.co.comcast.net] has joined #scala
[22:52:48] <xeno_by> which is effectively impossible without the help from the compiler itself
[22:53:11] <xeno_by> there's an alternative that involves a ToolBox though
[22:53:11] <TheChistoso> in my case that's actually not an issue since the compiler is a dependency :D
[22:53:20] <xeno_by> ah okay
[22:53:44] mary5030 [~mary5030@107-193-220-128.lightspeed.austtx.sbcglobal.net] has joined #scala
[22:55:05] mengu [~mengu@unaffiliated/mengu] has quit IRC: Ping timeout: 264 seconds
[22:55:52] <TheChistoso> how would i achieve this using the compiler for help? and do you ever see this being added to scala-reflect?
[22:57:01] obcode [~obcode@ob.cs.hm.edu] has joined #scala
[22:57:51] chatsiri_ [~chatsiri_@158.108.226.153] has joined #scala
[22:58:09] <xeno_by> okay i'm back
[22:58:15] <xeno_by> answering the first question
[22:58:26] <xeno_by> toolbox has the typecheck method
[22:58:43] <xeno_by> and typecheck does overload resolution, type inference and more
[22:58:54] <xeno_by> so you can create a dummy tree that represents and invocation of a constructor
[22:59:01] <xeno_by> and give it to the toolbox to do the typecheck
[22:59:07] <xeno_by> afterwards you get the result
[22:59:13] <xeno_by> extract a symbol
[22:59:18] <xeno_by> that has been assigned to that tree
[22:59:21] <xeno_by> and proceed
[22:59:35] <xeno_by> the tree in question would be the following
[22:59:42] mary5030 [~mary5030@107-193-220-128.lightspeed.austtx.sbcglobal.net] has quit IRC: Remote host closed the connection
[23:00:49] <xeno_by> val dummyArgs = args.map(arg => q"???: ${mirror.classSymbol(arg.getClass).asClass.toType}")
[23:01:01] <xeno_by> oops
[23:01:05] <xeno_by> no
[23:01:06] <xeno_by> no oops
[23:01:18] <xeno_by> q"new $yourClassSymbol(..$dummyArgs)"
[23:01:29] <xeno_by> afterwards you feed the tree into the toolbox
[23:01:40] <xeno_by> and then do result.symbol
[23:01:54] <xeno_by> of course, mirror.classSymbol(arg.getClass).asClass.toType is a sloppy way of doing things
[23:01:59] <xeno_by> as it will NPE on nulls
[23:02:05] <xeno_by> and might not work well with polymorphic types
[23:02:15] <xeno_by> but that can be figured out I guess
[23:02:33] <xeno_by> also I think there might be some permanent problems that follow from erasure
[23:02:52] <xeno_by> quite probably in some cases you won't be able to typecheck the ctor call
[23:03:00] <xeno_by> e.g. here
[23:03:19] <xeno_by> class C[T: ContextBound](list: List[T])
[23:03:35] <xeno_by> you won't know T, so you won't be able to infer the context bound
[23:03:57] <xeno_by> to fix that you would need to have TypeTags for your args
[23:04:13] <xeno_by> but that might not be an option if args come from Java
[23:04:24] boogie [~boogie@ip68-101-218-78.sd.sd.cox.net] has joined #scala
[23:04:32] DCC083 [~RedPunch@cpe-98-156-77-166.kc.res.rr.com] has quit IRC: Ping timeout: 272 seconds
[23:05:15] <TheChistoso> the args would be provided in Scala -- but i'm also fine w/ placing a restriction that these types cannot have type parameters
[23:05:24] <xeno_by> allright
[23:05:28] <xeno_by> then you should be fine
[23:05:34] <xeno_by> speaking of the second question
[23:05:39] <xeno_by> yes, I'm planning to do that
[23:05:57] <xeno_by> but it's not going to happen soon, as I have other things to do first
[23:06:00] <xeno_by> and it's not approved by Typesafe
[23:06:09] <xeno_by> so it might end up being rejected
[23:06:28] <xeno_by> but back then people were recognizing the necessity of doing exactly the things you're talking abou
[23:06:39] <TheChistoso> what's the reasoning behind that? seems like it would be a reasonable thing to want to do this
[23:06:45] <xeno_by> therefore I hope there won't be opposition when I actually implement something like that
[23:07:12] <xeno_by> by "not approved" I mean "not yet discussed with Typesafe and therefore not yet approved" :)
[23:07:28] Agro [~agro@108-79-20-223.lightspeed.hstntx.sbcglobal.net] has quit IRC: Quit: Leaving
[23:07:56] Agro [~agro@108-79-20-223.lightspeed.hstntx.sbcglobal.net] has joined #scala
[23:08:25] <TheChistoso> so tyvm of course -- another question -- what's the best way to determine if i have a class or module (given a string)?
[23:08:52] <xeno_by> where does the string come from?
[23:09:06] boogie [~boogie@ip68-101-218-78.sd.sd.cox.net] has quit IRC: Ping timeout: 252 seconds
[23:10:15] tpolecat [~tpolecat@c-76-27-230-159.hsd1.or.comcast.net] has quit IRC: Quit: is having a little nap now.
[23:11:06] <TheChistoso> it's a parameter for the method -- in my case i have source that gets compiled at runtime, i intercept some of the compiler phases looking for classes that implement requested traits, and the string ends up representing the full path of a discovered class
[23:12:14] boogie [~boogie@ip68-101-218-78.sd.sd.cox.net] has joined #scala
[23:16:12] <xeno_by> okay back again
[23:16:22] tpolecat [~tpolecat@c-76-27-230-159.hsd1.or.comcast.net] has joined #scala
[23:16:33] <xeno_by> i guess you'll have to try both staticClass and staticModule
[23:16:50] <xeno_by> because the same string could be both a class and a module name
[23:17:12] boogie [~boogie@ip68-101-218-78.sd.sd.cox.net] has quit IRC: Ping timeout: 272 seconds
[23:17:14] <xeno_by> if you inject into the compiler, then you might want to capture the Name that's used to represent these things internally
[23:17:24] <xeno_by> names can be TermName and TypeName
[23:17:34] <xeno_by> which immediately tells you whether it's a class or a module
[23:18:09] <TheChistoso> TermName would indicate a module?
[23:18:25] <xeno_by> yes
[23:19:46] <TheChistoso> another question -- the quasi quotes interpolator ("q") isn't being picked up. did i miss an import?
[23:19:56] <xeno_by> import c.universe._
[23:20:04] <xeno_by> import c.universe.Quasiquotes
[23:20:09] <xeno_by> or sth like that
[23:20:43] <TheChistoso> this isn't in a macro -- can i still do it?
[23:21:00] <xeno_by> sure you can
[23:21:03] <xeno_by> just not in 2.10
[23:21:09] <TheChistoso> ha :D
[23:21:12] <xeno_by> unless you have paradise plugin enabled
[23:21:26] hzhao [~hzhao@107-212-41-135.lightspeed.sntcca.sbcglobal.net] has quit IRC: Remote host closed the connection
[23:21:30] <TheChistoso> well i'm in 2.10 -- for this i don't mind bleeding edge though
[23:21:33] <xeno_by> with paradise on, you can have quasiquotes in both macros and runtime reflection
[23:22:00] hzhao [~hzhao@66.162.159.254] has joined #scala
[23:22:04] <TheChistoso> if i move to 2.11 (whatever the latest milestone is), can i get it w/o paradise?
[23:22:11] <xeno_by> yes you can
[23:22:19] <xeno_by> in 2.11 quasiquotes are part of Scala
*/