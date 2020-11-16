package controllers

import javax.inject.{Inject, Singleton}
import models.formapp.Enquete
import play.api.mvc.{AbstractController, ControllerComponents, Result}
import models.{Task, Tasks}
import models.{User, Users}
import utility.Digest

/**
  * TodoListコントローラ
  */
@Singleton
class TodoListController @Inject()(tasks: Tasks)(users: Users)(cc: ControllerComponents)
    extends AbstractController(cc) {

  /**
    *
    * @return
    */
  def index = Action { implicit request =>
    // 200 OK ステータスで app/views/index.scala.html をレンダリングする
    Ok(views.html.index("Welcome to Play application!")(request))
  }

  def list = Action { request =>
    val username = request.session.get("user::username")
    username match {
      case Some(username) =>
        val entries = tasks.listByUser(username)
        Ok(views.html.list(entries)(request))
      case None => Ok(views.html.list(Seq[Task]())(request))
    }

  }

  def entry(id: Int) = Action { request =>
    request.session.get("user::username") match {
      case Some(username) =>
        tasks.findByIDAndUsername(id, username) match {
          case Some(e) => Ok(views.html.entry(e))
          case None    => NotFound(views.html.simple(s"Cannot get entry for id=${id}")(username))
        }
      case None => Ok(views.html.simple("ログインしてください")(""))
    }
  }

  def startComplete(id: Int) = Action { request =>
    request.session.get("user::username") match {
      case Some(username) =>
        tasks.findByIDAndUsername(id, username) match {
          case Some(e) => Ok(views.html.completeForm(e)(request))
          case None    => NotFound(views.html.simple(s"Cannot get entry for id=${id}")(username))
        }
      case None => Redirect("/login")
    }
  }

  def complete(id: Int) = Action { request =>
    request.session.get("user::username") match {
      case Some(_) =>
        tasks.updateStatus(id, true)
        Redirect("/tasks")
      case None => Redirect("/login")
    }
  }

  def startUpdate(id: Int) = Action { request =>
    request.session.get("user::username") match {
      case Some(username) =>
        tasks.findByIDAndUsername(id, username) match {
          case Some(e) => Ok(views.html.updateForm(e)(request))
          case None    => NotFound(views.html.simple(s"Cannot get entry for id=${id}")(username))
        }
      case None => Redirect("/login")
    }
  }

  def update(id: Int) = Action { request =>
    request.session.get("user::username") match {
      case Some(_) =>
        (for {
          username    <- request.session.get("user::username")
          param       <- request.body.asFormUrlEncoded
          title       <- param.get("title").flatMap(_.headOption)
          description <- param.get("description").flatMap(_.headOption)
        } yield {
          tasks.save(Task(id, title, description, false, username, null))
          Redirect("/tasks")
        }).getOrElse[Result](Redirect("/register"))
      case None => Redirect("/login")
    }
  }

  def startDeletion(id: Int) = Action { request =>
    request.session.get("user::username") match {
      case Some(username) =>
        tasks.findByIDAndUsername(id, username) match {
          case Some(e) => Ok(views.html.deleteForm(e)(request))
          case None    => NotFound(views.html.simple(s"Cannot get entry for id=${id}")(username))
        }
      case None => Redirect("/login")
    }
  }

  def deletion(id: Int) = Action { request =>
    request.session.get("user::username") match {
      case Some(_) =>
        tasks.deletebyID(id)
        Redirect("/tasks")
      case None => Redirect("/login")
    }
  }

  def startRegistration = Action { request =>
    request.session.get("user::username") match {
      case Some(_) =>
        Ok(views.html.titleForm(request))
      case None => Redirect("/login")
    }
  }

  def registerTitle = Action { request =>
    (for {
      param <- request.body.asFormUrlEncoded
      title <- param.get("title").flatMap(_.headOption)
    } yield {
      Ok(views.html.descriptionForm(request)).withSession(request.session + ("todo::title" -> title))
    }).getOrElse[Result](Redirect("/register"))
  }

  def startLogin = Action { request =>
    request.session.get("user::username") match {
      case Some(username) => Ok(views.html.simple("既にログイン済みです")(username))
      case None           => Ok(views.html.loginForm(request)).withNewSession
    }
  }

  def login = Action { request =>
    (for {
      param    <- request.body.asFormUrlEncoded
      username <- param.get("username").flatMap(_.headOption)
      password <- param.get("password").flatMap(_.headOption)
    } yield {
      users.findByUsername(username) match {
        case Some(e) =>
          if (Digest(password) == e.password) {
            Redirect("/tasks").withNewSession.withSession(("user::username" -> username)) //ログイン後にセッションを再発行しておく
          } else {
            Redirect("/login")
          }
        case None =>
          Redirect("/login")
      }
    }).getOrElse[Result](Redirect("/login"))
  }

  def startSignup = Action { request =>
    Ok(views.html.signupForm(request))
  }

  def signup = Action { request =>
    (for {
      param    <- request.body.asFormUrlEncoded
      username <- param.get("username").flatMap(_.headOption)
      password <- param.get("password").flatMap(_.headOption)
    } yield {
      users.findByUsername(username) match {
        case Some(_) => Ok(views.html.simple(s"username=${username} already exists")(""))
        case None =>
          users.save(User(username, Digest(password)))
          Redirect("/tasks").withNewSession.withSession(("user::username" -> username))
      }
    }).getOrElse[Result](Redirect("/tasks"))
  }

  def startSetting = Action { request =>
    request.session.get("user::username") match {
      case Some(_) => Ok(views.html.settingForm(request))
      case None    => Redirect("/login")
    }
  }

  def startNewPassword = Action { request =>
    request.session.get("user::username") match {
      case Some(_) => Ok(views.html.newPasswordForm(request))
      case None    => Redirect("/login")
    }
  }

  def newPassword = Action { request =>
    request.session.get("user::username") match {
      case Some(username) =>
        (for {
          param          <- request.body.asFormUrlEncoded
          prev_password  <- param.get("prev_password").flatMap(_.headOption)
          password       <- param.get("password").flatMap(_.headOption)
          password_check <- param.get("password_check").flatMap(_.headOption)
        } yield {
          users.findByUsername(username) match {
            case Some(e) =>
              if (Digest(prev_password) == e.password && password == password_check) {
                users.updatePassword(User(username, Digest(password)))
                Ok(views.html.simple("パスワードを変更しました")(username))
              } else {
                Redirect("/settings/newPassword")
              }
            case None => Redirect("/login").withNewSession
          }
        }).getOrElse[Result](Redirect("/tasks"))
      case None => Redirect("/login")
    }
  }

  def startRemoveUser = Action { request =>
    request.session.get("user::username") match {
      case Some(_) => Ok(views.html.removeUserForm(request))
      case None    => Redirect("/login")
    }
  }

  def removeUser = Action { request =>
    request.session.get("user::username") match {
      case Some(username) =>
        //userの持つタスクを削除してからuserを削除する
        tasks.deletebyUsername(username)
        users.delete(username)
        Ok(views.html.simple("ユーザの削除が完了しました")("")).withNewSession
      case None => Redirect("/login")
    }
  }

  def logout = Action { request =>
    Redirect("/tasks").withNewSession
  }

  def registerDescription = Action { request =>
    (for {
      username    <- request.session.get("user::username")
      title       <- request.session.get("todo::title")
      param       <- request.body.asFormUrlEncoded
      description <- param.get("description").flatMap(_.headOption)
    } yield {
      val task = Task(title, description, false, username)
      Ok(views.html.confirm(task)(request))
        .withSession(request.session + ("todo::description" -> description))
    }).getOrElse[Result](Redirect("/register"))
  }

  def confirm = Action { request =>
    request.session.get("user::username") match {
      case Some(_) =>
        (for {
          username    <- request.session.get("user::username")
          title       <- request.session.get("todo::title")
          description <- request.session.get("todo::description")
        } yield {
          tasks.save(Task(title, description, false, username))
          Redirect("/tasks").withNewSession.withSession(("user::username" -> username)) //提出したらセッションはリセットする
        }).getOrElse[Result](Redirect("/register"))
      case None => Redirect("/login")
    }
  }

  def hello = Action { implicit request =>
    Ok("Hello world!")
  }

  def hello_name(name: String) = Action { implicit request =>
    // 200 OK ステータスで app/views/index.scala.html をレンダリングする
    Ok(s"Hello world, $name!")
  }
}
