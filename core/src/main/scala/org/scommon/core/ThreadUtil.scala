package org.scommon.core

import java.util.concurrent.Callable

import scala.language.implicitConversions

object ThreadUtil {
  type EmptyFunction = () => Unit

  implicit def unit2Callable(fn: => Unit): Callable[Unit] = new Callable[Unit] {
    def call(): Unit = fn
  }

  implicit def fn2Callable(fn:EmptyFunction): Callable[Unit] = new Callable[Unit] {
    def call(): Unit = fn()
  }
}