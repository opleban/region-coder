package com.socrata.geospace.lib.regioncache

import com.rojoma.json.v3.ast.{JString, JValue}
import com.socrata.geospace.lib.feature.FeatureExtensions
import FeatureExtensions._
import com.socrata.geospace.lib.client.SodaResponse
import com.socrata.soda.external.SodaFountainClient
import com.socrata.geospace.lib.client.GeoToSoda2Converter
import com.socrata.thirdparty.geojson.FeatureJson
import com.typesafe.config.Config
import org.geoscript.feature.Feature
import scala.util.Success
import com.rojoma.json.v3.ast.{JValue, JString, JArray, JObject}

/**
  * Caches indices of the region datasets for geo-region-coding in a hashmap
  * for simple string matching.
  * @param config Cache configuration
  */
class LabelCache(config: Config) extends RegionCache[Map[Int, String]](config) {

    override protected def getEntryFromFeatureJson(features: Seq[com.socrata.thirdparty.geojson.FeatureJson],
        resourceName: String,
        keyAttribute: String,
        valueAttribute: String): Map[Int,String] = Map.empty[Int, String]

    override protected def getEntryFromFeatures(features: Seq[org.geoscript.feature.Feature],
        keyName: String): Map[Int,String] = Map.empty[Int, String]

  /**
    * Generates an in-memory map for the dataset given the set of features
    * @param features Features from which to generate a SpatialIndex
    * @return SpatialIndex containing the dataset features
    */
  def constructHashMap(sodaFountain: SodaFountainClient,
                                resourceName: String,
                                labelColumnToReturn: String,
                                idColumnToReturn: String): Map[Int, String] = {
    val sodaResponse = sodaReadTimer.time {
      sodaFountain.query(resourceName,
                        Some("json"),
                        Iterable(("$query",
                        s"select $labelColumnToReturn as _label, $idColumnToReturn as _id")))
    }
    // Originally using javax lib for this one status code, I doubt highly it will ever change, and
    // we will avoid having to make an import for single item by statically adding it.
    val payload = SodaResponse.check(sodaResponse, StatusOK)
    val exception = new RuntimeException(s"dataset $resourceName contains a feature with missing" +
                          s"$labelColumnToReturn/$idColumnToReturn property")
    payload.toOption
      .flatMap { jValue =>
        jValue match {
          case JArray(rows) =>
            val t = rows.collect { case JObject(obj) =>
                logger.info("{}", obj)
                (obj.get("_id"), obj.get("_label")) match {
                  case (Some(JString(k)), Some(JString(v))) => Map(k.toInt -> v)
                  case _ => throw exception
                }
              }
              Option(t)
          case _ => throw exception
        }
      }
      .getOrElse {
        val errMsg = "Could not read GeoJSON from soda fountain: " + payload.get
        if (payload.isFailure) { throw new RuntimeException(errMsg, payload.failed.get) }
        else                   { throw new RuntimeException(errMsg) }
      }.flatten.toMap
  }

}
