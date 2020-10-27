package controllers.formapp

import javax.inject.{Inject, Singleton}

import play.api.mvc.{AbstractController, ControllerComponents, Result}
import models.formapp.{Enquete, Enquetes}

/**
  * 前半課題 formapp の Play Framework 上での実装
  */
@Singleton
class FormappController @Inject()(enquetes: Enquetes)(cc: ControllerComponents) extends AbstractController(cc) {

  /**
    *
    * @return
    */
  def list = Action { request =>
    val entries = enquetes.list
    Ok(views.html.formapp.list(entries))
  }

  def entry(id: Int) = Action {
    enquetes.findByID(id) match {
      case Some(e) => Ok(views.html.formapp.entry(e))
      case None    => NotFound(s"No entry for id=${id}")
    }
  }

  def startRegistration = Action { request =>
    Ok(views.html.formapp.nameForm(request)).withNewSession
  }

  def registerName = Action { request =>
    (for {
      param <- request.body.asFormUrlEncoded
      name  <- param.get("name").flatMap(_.headOption)
    } yield {
      Ok(views.html.formapp.genderForm(request)).withSession(request.session + ("formapp::name" -> name))
    }).getOrElse[Result](Redirect("/formapp/register"))
  }

  def registerGender = Action { request =>
    (for {
      _      <- request.session.get("formapp::name")
      param  <- request.body.asFormUrlEncoded
      gender <- param.get("gender").flatMap(_.headOption)
    } yield {
      Ok(views.html.formapp.messageForm(request)).withSession(request.session + ("formapp::gender" -> gender))
    }).getOrElse[Result](Redirect("/formapp/register"))
  }

  def registerMessage = Action { request =>
    (for {
      name    <- request.session.get("formapp::name")
      gender  <- request.session.get("formapp::gender")
      param   <- request.body.asFormUrlEncoded
      message <- param.get("message").flatMap(_.headOption)
    } yield {
      val enquete = Enquete(name, gender, message)
      Ok(views.html.formapp.confirm(enquete)(request)).withSession(request.session + ("formapp::message" -> message))
    }).getOrElse[Result](Redirect("/formapp/register"))
  }

  def confirm = Action { request =>
    (for {
      name    <- request.session.get("formapp::name")
      gender  <- request.session.get("formapp::gender")
      message <- request.session.get("formapp::message")
    } yield {
      enquetes.save(Enquete(name, gender, message))
      Redirect("/formapp/messages").withNewSession
    }).getOrElse[Result](Redirect("/formapp/register"))
  }

}
