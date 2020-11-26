package controllers

import java.util.regex.Pattern

import javax.inject.{Inject, Singleton}
import models.{Task, Tasks, User, Users}
import play.api.mvc._
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
  def index: Action[AnyContent] = Action { implicit request =>
    // 200 OK ステータスで app/views/index.scala.html をレンダリングする
    Ok(views.html.index("Welcome to Play application!")(request))
  }

  def list: Action[AnyContent] = Action { request =>
    val username = request.session.get("user::username")
    username match {
      case Some(username) =>
        val entries = tasks.listByUser(username)
        Ok(views.html.list(entries)(request))
      case None => Ok(views.html.list(Seq[Task]())(request))
    }

  }

  def entry(id: Int): Action[AnyContent] = Action { request =>
    val username = request.session.get("user::username")
    users.findByUsername(username) match {
      case Some(user) =>
        tasks.findByID(id) match {
          case Some(e) =>
            if (e.username == user.username) {
              Ok(views.html.entry(e))
            } else {
              Unauthorized(views.html.simple(s"id=$id は許可されていません")(user.username))
            }
          case None => NotFound(views.html.simple(s"id=$id は見つかりません")(user.username))
        }
      case None => Unauthorized(views.html.simple("ログインしてください")(""))
    }
  }

  def startComplete(id: Int): Action[AnyContent] = Action { request =>
    val username = request.session.get("user::username")
    users.findByUsername(username) match {
      case Some(user) =>
        tasks.findByID(id) match {
          case Some(e) =>
            if (e.username == user.username) {
              Ok(views.html.completeForm(e)(request))
            } else {
              Unauthorized(views.html.simple(s"id=$id は許可されていません")(user.username))
            }
          case None => NotFound(views.html.simple(s"id=$id は見つかりません")(user.username))
        }
      case None => Unauthorized(views.html.simple("ログインしてください")(""))
    }
  }

  def complete(id: Int): Action[AnyContent] = Action { request =>
    val username = request.session.get("user::username")
    users.findByUsername(username) match {
      case Some(_) =>
        tasks.updateStatus(id, is_done = true)
        Redirect("/tasks")
      case None => Unauthorized(views.html.simple("ログインしてください")(""))
    }
  }

  def startUpdate(id: Int): Action[AnyContent] = Action { request =>
    val username = request.session.get("user::username")
    users.findByUsername(username) match {
      case Some(user) =>
        tasks.findByID(id) match {
          case Some(e) =>
            if (e.username == user.username) {
              Ok(views.html.updateForm(e)(request))
            } else {
              Unauthorized(views.html.simple(s"id=$id は許可されていません")(user.username))
            }
          case None => NotFound(views.html.simple(s"id=$id は見つかりません")(user.username))
        }
      case None => Unauthorized(views.html.simple("ログインしてください")(""))
    }
  }

  def update(id: Int): Action[AnyContent] = Action { request =>
    val username = request.session.get("user::username")
    users.findByUsername(username) match {
      case Some(_) =>
        (for {
          username    <- request.session.get("user::username")
          param       <- request.body.asFormUrlEncoded
          title       <- param.get("title").flatMap(_.headOption)
          description <- param.get("description").flatMap(_.headOption)
        } yield {
          if (title.length <= 32 && description.length < 256) {
            tasks.save(Task(id, title, description, isDone = false, username, null))
            Redirect("/tasks")
          } else {
            BadRequest(views.html.simple("タイトルは32文字以下，説明は255文字以下です．")(username))
          }
        }).getOrElse[Result](Redirect("/register"))
      case None => Unauthorized(views.html.simple("ログインしてください")(""))
    }
  }

  def startDeletion(id: Int): Action[AnyContent] = Action { request =>
    val username = request.session.get("user::username")
    users.findByUsername(username) match {
      case Some(user) =>
        tasks.findByID(id) match {
          case Some(e) =>
            if (e.username == user.username) {
              Ok(views.html.deleteForm(e)(request))
            } else {
              Unauthorized(views.html.simple(s"id=$id は許可されていません")(user.username))
            }
          case None => NotFound(views.html.simple(s"id=$id は見つかりません")(user.username))
        }
      case None => Unauthorized(views.html.simple("ログインしてください")(""))
    }
  }

  def deletion(id: Int): Action[AnyContent] = Action { request =>
    val username = request.session.get("user::username")
    users.findByUsername(username) match {
      case Some(_) =>
        tasks.deleteByID(id)
        Redirect("/tasks")
      case None => Unauthorized(views.html.simple("ログインしてください")(""))
    }
  }

  def startRegistration: Action[AnyContent] = Action { request =>
    val username = request.session.get("user::username")
    users.findByUsername(username) match {
      case Some(_) =>
        Ok(views.html.titleForm(request))
      case None => Unauthorized(views.html.simple("ログインしてください")(""))
    }
  }

  def registerTitle: Action[AnyContent] = Action { request =>
    (for {
      username <- request.session.get("user::username")
      param    <- request.body.asFormUrlEncoded
      title    <- param.get("title").flatMap(_.headOption)
    } yield {
      if (title.length <= 32) {
        Ok(views.html.descriptionForm(request)).withSession(request.session + ("todo::title" -> title))
      } else {
        BadRequest(views.html.simple("タイトルは32文字以下，説明は255文字以下です．")(username))
      }
    }).getOrElse[Result](Redirect("/register"))
  }

  def registerDescription: Action[AnyContent] = Action { request =>
    (for {
      username    <- request.session.get("user::username")
      title       <- request.session.get("todo::title")
      param       <- request.body.asFormUrlEncoded
      description <- param.get("description").flatMap(_.headOption)
    } yield {
      val task = Task(title, description, isDone = false, username)
      if (title.length <= 32 && description.length < 256) {
        Ok(views.html.confirm(task)(request))
          .withSession(request.session + ("todo::description" -> description))
      } else {
        BadRequest(views.html.simple("タイトルは32文字以下，説明は255文字以下です．")(username))
      }
    }).getOrElse[Result](Redirect("/register"))
  }

  def confirm: Action[AnyContent] = Action { request =>
    val username = request.session.get("user::username")
    users.findByUsername(username) match {
      case Some(_) =>
        (for {
          username    <- request.session.get("user::username")
          title       <- request.session.get("todo::title")
          description <- request.session.get("todo::description")
        } yield {
          if (title.length <= 32 && description.length < 256) {
            tasks.save(Task(title, description, isDone = false, username))
            Redirect("/tasks").withNewSession.withSession("user::username" -> username) //提出したらセッションはリセットする
          } else {
            BadRequest(views.html.simple("タイトルは32文字以下，説明は255文字以下です．")(username))
          }
        }).getOrElse[Result](Redirect("/register"))
      case None => Unauthorized(views.html.simple("ログインしてください")(""))
    }
  }
  def startLogin: Action[AnyContent] = Action { request =>
    val username = request.session.get("user::username")
    users.findByUsername(username) match {
      case Some(user) => Ok(views.html.simple("既にログイン済みです")(user.username))
      case None       => Ok(views.html.loginForm(request)).withNewSession
    }
  }

  def login: Action[AnyContent] = Action { request =>
    (for {
      param    <- request.body.asFormUrlEncoded
      username <- param.get("username").flatMap(_.headOption)
      password <- param.get("password").flatMap(_.headOption)
    } yield {
      users.findByUsername(Some(username)) match {
        case Some(e) =>
          if (Digest(password) == e.password) {
            Redirect("/tasks").withNewSession.withSession("user::username" -> username) //ログイン後にセッションを再発行しておく
          } else {
            BadRequest(views.html.simple("ユーザ名かパスワードか間違っているか，ユーザが存在しません")(""))
          }
        case None => BadRequest(views.html.simple("ユーザ名かパスワードか間違っているか，ユーザが存在しません")(""))
      }
    }).getOrElse[Result](Redirect("/login"))
  }

  def startSignup: Action[AnyContent] = Action { request =>
    Ok(views.html.signupForm(request))
  }

  def signup: Action[AnyContent] = Action { request =>
    (for {
      param          <- request.body.asFormUrlEncoded
      username       <- param.get("username").flatMap(_.headOption)
      password       <- param.get("password").flatMap(_.headOption)
      password_check <- param.get("password_check").flatMap(_.headOption)
    } yield {
      users.findByUsername(Some(username)) match {
        case Some(_) =>
          BadRequest(views.html.simple(s"username=$username already exists")(""))
        case None =>
          if (password == password_check) {
            if (username.length <= 32 && password.length <= 255 && Pattern
                  .matches("^[0-9a-zA-Z]+$", username) && Pattern
                  .matches("^[a-zA-Z0-9!-/:-@\\[-`{-~]+$", password)) {
              users.save(User(username, Digest(password)))
              Redirect("/tasks").withNewSession.withSession("user::username" -> username)
            } else {
              BadRequest(views.html.simple(s"ユーザ名は半角英数(<=32)のみ，パスワードは半角英数記号(<=255)のみ入力可能です")(""))
            }
          } else {
            BadRequest(views.html.simple(s"パスワードが一致していません")(""))
          }
      }
    }).getOrElse[Result](Redirect("/tasks"))
  }

  def startSetting: Action[AnyContent] = Action { request =>
    val username = request.session.get("user::username")
    users.findByUsername(username) match {
      case Some(_) =>
        Ok(views.html.settingForm(request))
      case None => Unauthorized(views.html.simple("ログインしてください")(""))
    }
  }

  def startNewPassword: Action[AnyContent] = Action { request =>
    val username = request.session.get("user::username")
    users.findByUsername(username) match {
      case Some(_) => Ok(views.html.newPasswordForm(request))
      case None    => Unauthorized(views.html.simple("ログインしてください")(""))
    }
  }

  def newPassword: Action[AnyContent] = Action { request =>
    val username = request.session.get("user::username")
    users.findByUsername(username) match {
      case Some(user) =>
        (for {
          param          <- request.body.asFormUrlEncoded
          prev_password  <- param.get("prev_password").flatMap(_.headOption)
          password       <- param.get("password").flatMap(_.headOption)
          password_check <- param.get("password_check").flatMap(_.headOption)
        } yield {
          users.findByUsername(Some(user.username)) match {
            case Some(e) =>
              if (Digest(prev_password) == e.password) {
                if (password == password_check) {
                  if (password.length <= 255 && Pattern
                        .matches("^[a-zA-Z0-9!-/:-@\\[-`{-~]+$", password)) {
                    users.updatePassword(User(user.username, Digest(password)))
                    Ok(views.html.simple("パスワードを変更しました")(user.username))
                  } else {
                    BadRequest(views.html.simple(s"パスワードは半角英数記号(<=255)のみ入力可能です")(""))
                  }
                } else {
                  BadRequest(views.html.simple(s"パスワードが一致していません")(""))
                }
              } else {
                BadRequest(views.html.simple(s"パスワードが合っていません")(""))
              }
            case None => Redirect("/login").withNewSession
          }
        }).getOrElse[Result](Redirect("/tasks"))
      case None => Unauthorized(views.html.simple("ログインしてください")(""))
    }
  }

  def startRemoveUser: Action[AnyContent] = Action { request =>
    val username = request.session.get("user::username")
    users.findByUsername(username) match {
      case Some(_) => Ok(views.html.removeUserForm(request))
      case None    => Unauthorized(views.html.simple("ログインしてください")(""))
    }
  }

  def removeUser(): Action[AnyContent] = Action { request =>
    val username = request.session.get("user::username")
    users.findByUsername(username) match {
      case Some(user) =>
        //userの持つタスクを削除してからuserを削除する
        tasks.deleteByUsername(user.username)
        users.delete(user.username)
        Ok(views.html.simple("ユーザの削除が完了しました")("")).withNewSession
      case None => Unauthorized(views.html.simple("ログインしてください")(""))
    }
  }

  def logout: Action[AnyContent] = Action { _ =>
    Redirect("/tasks").withNewSession
  }

  def hello: Action[AnyContent] = Action { implicit request =>
    Ok("Hello world!")
  }

  def hello_name(name: String): Action[AnyContent] = Action { implicit request =>
    // 200 OK ステータスで app/views/index.scala.html をレンダリングする
    Ok(s"Hello world, $name!")
  }
}
