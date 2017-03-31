package controllers

import play.api.Play.current
import scala.concurrent.{ ExecutionContext, Future, Promise }
import javax.inject._
import play.api.mvc.Controller
import play.api.libs.ws._
import akka.actor.ActorSystem
import play.api.mvc.Action
import play.api.libs.json.Json
import play.api.libs.json.Reads._
import play.api.libs.json.util._
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads
import play.api.libs.json._

case class lev0(lcao24: String, callsign: String, origin_country: String, time_position: String,
                time_velocity: String, longitude: String, latitude: String, altitude: String, on_ground: String,
                velocity: String, heading: String, vertical_rate: String, sensors: String)
//object lev0 extends Function0[String] {
//  def decomposer(lcao24: String, callsign: String, origin_country: String, time_position: String,
//    time_velocity: String, longitude: String, latitude: String, altitude: String, on_ground: String,
//    velocity: String, heading: String, vertical_rate: String, sensors: String): lev0 = lev0(lcao24, callsign, origin_country, time_position, time_velocity,
//    longitude, latitude, altitude, on_ground, velocity, heading,
//    vertical_rate, sensors)
//  implicit val lev0reader = (
//    (__ \ "lcao24").read[String] and  
//    (__ \ "callsign").read[String] and
//    (__ \ "origin_country").read[String] and
//    (__ \ "time_position").read[String] and
//    (__ \ "time_velocity").read[String] and
//    (__ \ "longitude").read[String] and
//    (__ \ "latitude").read[String] and
//    (__ \ "altitude").read[String] and
//    (__ \ "on_ground").read[String] and
//    (__ \ "velocity").read[String] and
//    (__ \ "heading").read[String] and
//    (__ \ "vertial_rate").read[String] and
//    (__ \ "sensors").read[String])(unlift(decomposer _))
//}
//
case class lev1(time: Int, states: lev0)
//object lev1 extends Function1[Int, lev0] {
//  def decomposer(time:Int,states:lev0)=lev1(time,states)
//  implicit val lev1reader = (
//    (__ \ "time").read[Int] and
//    (__ \ "states").read[lev0])(unlift(decomposer _))
//}

@Singleton
class RestHandler @Inject() (actorSystem: ActorSystem)(implicit exec: ExecutionContext) extends Controller {
  implicit val timestamp: Long = System.currentTimeMillis / 1000
  implicit val lev0reader: Reads[lev0] = (
    (__ \ "lcao24").read[String] and
    (__ \ "callsign").read[String] and
    (__ \ "origin_country").read[String] and
    (__ \ "time_position").read[String] and
    (__ \ "time_velocity").read[String] and
    (__ \ "longitude").read[String] and
    (__ \ "latitude").read[String] and
    (__ \ "altitude").read[String] and
    (__ \ "on_ground").read[String] and
    (__ \ "velocity").read[String] and
    (__ \ "heading").read[String] and
    (__ \ "vertial_rate").read[String] and
    (__ \ "sensors").read[String])(lev0.apply _)
  implicit val lev1reader: Reads[lev1] = (
    (__ \ "time").read[Int] and
    (__ \ "states").read[lev0])(lev1.apply _)

  val googleAPIKey = "AIzaSyB8etD7igah1abQsgAMRBmftkSVcEgVy5U"

  private def getLoc(): Future[List[String]] = {

    val openskyurl = "https://opensky-network.org/api/states/all"
    val raw = WS.url(openskyurl).withAuth("sandeep1729", "skyfall", WSAuthScheme.BASIC).withQueryString(("time", timestamp.toString)).get()
    val response = raw map { point =>
      val x = Json.parse(point.body)
      //x.asInstanceOf[lev1]

      //x.asOpt[lev1]
      (x \\ "states").asInstanceOf[List[String]]
    }
    response
  }
  def getAll() = Action.async {

    implicit req =>

      for {
        place <- getLoc()
      } yield {
        val js = Json.obj("time" -> timestamp, "time2" -> place.mkString(""))

        Ok(js)
      }
  }

  val regex = "[^0-9,.-]".r
  def findLocation(airport: String) = Action.async {

    implicit req =>

      for {
        geoloc <- locationRetreiver(airport)
      } yield {
        val coord = regex.replaceAllIn(geoloc.mkString(""), "")
        val js = Json.obj("time" -> timestamp, "location" -> coord)

        Ok(js)
      }
  }

  def locationRetreiver(airport: String) = {
    val apiKey = "AIzaSyB8etD7igah1abQsgAMRBmftkSVcEgVy5U"
    val gmapsurl = "https://maps.googleapis.com/maps/api/geocode/json?"
    val raw = WS.url(gmapsurl).withHeaders("key" -> apiKey).withQueryString(("address", airport)).get()
    val response = raw map { point =>
      val x = Json.parse(point.body)
      //x.asInstanceOf[lev1]
      //x
      (x \\ "location").asInstanceOf[List[String]]
    }
    response
  }

}