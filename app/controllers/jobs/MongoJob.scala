package controllers.jobs

import play.modules.reactivemongo.ReactiveMongoPlugin
import play.api.Play.current

trait MongoJob{
  def db = ReactiveMongoPlugin.db
}

