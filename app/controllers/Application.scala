package controllers
  
import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json._
import play.api.libs.json.JsPath
import play.api.libs.json.Json.toJson
import play.api.libs.json.Reads
import play.api.libs.json.Reads._
import play.api.libs.json.Reads.StringReads
import play.api.libs.json.Reads.functorReads
import play.api.mvc.Action
import play.api.mvc.Controller
  
class Application extends Controller {
  
  // Shows the login screen and empties the session:
  def index = Action {
    Ok(views.html.index()).withNewSession
  }
  
  // Handles the username-password sent as JSON:
  def login = Action(parse.json) { request =>
  
    // Creates a reader for the JSON - turns it into a LoginRequest
    implicit val loginRequest: Reads[LoginRequest] = Json.reads[LoginRequest]
  
    /*
     * Call validate and if ok we return valid=true and put username in session
     */
    request.body.validate[LoginRequest] match {
      case s: JsSuccess[LoginRequest] if (s.get.authenticate) => {
        Ok(toJson(Map("valid" -> true))).withSession("user" -> s.get.username)
      }
      // Not valid
      case _ => Ok(toJson(Map("valid" -> false)))
    }
  }
  
  def dashboard = Action { implicit request =>
    request.session.get("user").map {
      user =>
        {
          Ok(views.html.dashboard(user))
        }
    }.getOrElse(Redirect(routes.Application.index()))
  }
}
  
case class LoginRequest(username: String, password: String) {
    
  // Simple username-password map in place of a database:
  val Users = Map("demo" -> "demo")
    
  def authenticate = Users.exists(_ == (username, password))
}