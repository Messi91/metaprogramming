package com.mesfin.meta.annotations

import scala.annotation._
import scala.collection.immutable
import scala.meta._

class Memoize extends StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    defn match {
      case q"def $name(...$paramss): $result = { ..$body }" =>
        val newBody = immutable.Seq(
          q"println(${"Macro hello"})"
        ) ++ body
        q"def $name(...$paramss): $result = { ..$newBody }"
      case _ => abort("@Memoize must annotate a function.")
    }
  }
}
