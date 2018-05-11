package com.mesfin.meta.annotations

import scala.annotation._
import scala.collection.immutable
import scala.meta._

class Memoize extends StaticAnnotation {

  def hashEvaluationTerm(paramss: Seq[Seq[Term.Param]]) = {
    paramss
      .flatten
      .map(_.name.value)
      .map(_ + ".hashCode().toString()")
      .mkString(" + ")
      .parse[Term].get
  }

  val cacheVar = q"$$cache"
  val hashVar = q"$$hash"
  val resultVar = q"$$result"
  def patName(termName: Term.Name): Pat.Var.Term =
    Pat.Var.Term(termName)

  inline def apply(defn: Any): Any = meta {
    defn match {
      case q"def $name(...$paramss): $result = { ..$body }" =>

        // Term to check is cache contains evaluated result
        val isCacheContainsResult = q"$cacheVar.containsKey($hashVar)"

        // Term to get result from cache
        val getResultFromCache = q"$cacheVar.get($hashVar)"

        // Term to evaluate result and put it in cache
        val evaluateFunction = immutable.Seq(
          q"val ${patName(resultVar)} = { ..$body }",
          q"$cacheVar.put($hashVar, $resultVar)",
          q"$resultVar"
        )

        // Term to evaluate hash and get result with it
        val evaluateHashAndGetResult = immutable.Seq(
          q"val ${patName(hashVar)} = ${hashEvaluationTerm(paramss)}",
          q"if($isCacheContainsResult) { $getResultFromCache } else { ..$evaluateFunction }"
        )

        // Term to build cache and anon func that will return result
        val initializeCacheAndBuildFunc = immutable.Seq(
          q"val ${patName(cacheVar)} = new java.util.concurrent.ConcurrentHashMap[String, Any]",
          q"((..${paramss.flatten}) => { ..$evaluateHashAndGetResult })"
        )

        q"val ${patName(name)} = { ..$initializeCacheAndBuildFunc }"

      case _ =>
        abort("@Memoize must annotate an function.")
    }
  }
}
