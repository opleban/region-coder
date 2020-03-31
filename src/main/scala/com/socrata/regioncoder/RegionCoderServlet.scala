package com.socrata.regioncoder

import javax.servlet.http.{HttpServletResponse => HttpStatus}

import com.rojoma.json.v3.ast.{JObject, JString}
import com.rojoma.json.v3.util.JsonUtil
import com.socrata.geospace.lib.Utils._
import com.socrata.regioncoder.config.RegionCoderConfig
import com.socrata.soda.external.SodaFountainClient
import org.scalatra.metrics.MetricsSupport
import org.scalatra.{AsyncResult, BadRequest, Ok}

// scalastyle:off multiple.string.literals
class RegionCoderServlet(rcConfig: RegionCoderConfig, val sodaFountain: SodaFountainClient)
  extends RegionCoderStack with RegionCoder with MetricsSupport {

  val cacheConfig = rcConfig.cache
  val partitionXsize = rcConfig.partitioning.sizeX
  val partitionYsize = rcConfig.partitioning.sizeY

  def pointcodeTimer[A](f: => A): A = timer("pointcode") {f}.call()
  def stringcodeTimer[A](f: => A): A = timer("stringcode") {f}.call()
  def debugGetTimer[A](f: => A): A = timer("debug-get") {f}.call()
  def debugClearTimer[A](f: => A): A = timer("debug-clear") {f}.call()

  get("/") {
    """{
      |"hello": "region-coder"
    }""".stripMargin
  }

  get("/version") {
    JsonUtil.renderJson(JObject(BuildInfo.toMap.mapValues(v => JString(v.toString))))
  }

  // Request body is a JSON array of points. Each point is an array of length 2.
  // Example: [[-87.6847,41.8369],[-122.3331,47.6097],...]
  post("/v2/regions/:resourceName/pointcode") {
    val points = parsedBody.extract[Seq[Seq[Double]]]
    if (points.isEmpty) {
      halt(HttpStatus.SC_BAD_REQUEST, s"Could not parse '${request.body}'.  Must be in the form [[x, y],[a,b],...]")
    }
    val columnToReturn = params.getOrElse("columnToReturn", halt(BadRequest("Missing param 'columnToReturn'")))

    new AsyncResult {
      override val timeout = rcConfig.shapePayloadTimeout
      val is = pointcodeTimer { regionCodeByPoint(params("resourceName"), columnToReturn, points) }
    }
  }

  // Request body is a JSON array of points. Each point is an array of length 2.
  // Example: [[-87.6847,41.8369],[-122.3331,47.6097],...]
  post("/v2/regions/:resourceName/transformcode") {
    val points = parsedBody.extract[Seq[Seq[Double]]]
    if (points.isEmpty) {
      halt(HttpStatus.SC_BAD_REQUEST, s"Could not parse '${request.body}'.  Must be in the form [[x, y],[a,b],...]")
    }
    val labelToReturn = params.getOrElse("labelToReturn", halt(BadRequest("Missing param 'columnToReturn'")))
    val idColumnToReturn = params.getOrElse("idColumnToReturn", halt(BadRequest("Missing param 'idColumnToReturn'")))
    new AsyncResult {
      override val timeout = rcConfig.shapePayloadTimeout
      val is = pointcodeTimer { regionCodeByTransform(params("resourceName"), labelToReturn, idColumnToReturn, points) }
    }
  }

  post("/v2/regions/:resourceName/stringcode") {
    val strings = parsedBody.extract[Seq[String]]
    if (strings.isEmpty) halt(HttpStatus.SC_BAD_REQUEST,
      s"""Could not parse '${request.body}'.  Must be in the form ["98102","98101",...]""")
    val columnToMatch = params.getOrElse("columnToMatch", halt(BadRequest("Missing param 'columnToMatch'")))
    val columnToReturn = params.getOrElse("columnToReturn", halt(BadRequest("Missing param 'columnToReturn'")))

    new AsyncResult {
      override val timeout = rcConfig.shapePayloadTimeout
      val is = stringcodeTimer { regionCodeByString(params("resourceName"), columnToMatch, columnToReturn, strings) }
    }
  }

  // DEBUGGING ROUTE : Returns a JSON blob with info about all currently cached regions, ordered by least-recently-used
  get("/:version/regions") {
    debugGetTimer {
      Map(
        "spatialCache" -> spatialCache.entriesByLeastRecentlyUsed().map {
          case (key, size) => Map("resource" -> key, "numCoordinates" -> size) },

        "stringCache" -> stringCache.entriesByLeastRecentlyUsed().map {
          case (key, size) => Map("resource" -> key, "numRows" -> size) })
    }
  }

  // DEBUGGING ROUTE : Clears the region cache
  delete("/:version/regions") {
    debugClearTimer {
      resetRegionState()
      logMemoryUsage("After clearing region caches")
      Ok("Done")
    }
  }
}
