package me.krobinson.twitter

import twitter4j._
import twitter4j.conf.ConfigurationBuilder
import com.typesafe.config.ConfigFactory

import collection.JavaConverters._
import scala.util.Try


object TwitterClient {
  def apply(): Twitter = {
    val conf = ConfigFactory.load()
    val cb = new ConfigurationBuilder()

    cb.setDebugEnabled(true)
      .setOAuthConsumerKey(conf.getString("twitter.oauth.consumer_key"))
      .setOAuthConsumerSecret(conf.getString("twitter.oauth.consumer_secret"))
      .setOAuthAccessToken(conf.getString("twitter.oauth.access_token"))
      .setOAuthAccessTokenSecret(conf.getString("twitter.oauth.access_token_secret"))

    val tf = new TwitterFactory(cb.build())
    tf.getInstance()
  }

  implicit class ResponseExtensions(t: Twitter) {

    private def buildResultSince(queryString: String, id: Long): QueryResult = {
      val q: Query = new Query(queryString)
      q.setCount(100)
      q.setSinceId(id)
      t.search(q)
    }

    def querySince(queryString: String, id: Long, retry: Int = 5): List[Status] = {
      val result: QueryResult = buildResultSince(queryString, id)
      val replies: List[Status] = result.getTweets.asScala.toList

      replies.size match {
        case c if c < 100 => replies
        case _            => replies ++ querySince(queryString, result.getMaxId, retry - 1)
      }
    }

    private def getMentions(id: Long, username: String, relevantIds: Set[Long]): List[Status] = {
      val mentions = querySince(s"@$username", id)

      mentions.filter { t =>
        val replyId = t.getInReplyToStatusId
        relevantIds.contains(replyId)
      }
    }

    private def getReplies(id: Long, username: String): List[Status] = {
      val retweets = t.tweets.getRetweets(id).iterator().asScala.toList
      val retweetIds: Set[Long] = retweets.map(_.getId).toSet

      val replies = querySince(s"to:$username", id)
      val replyIds = replies.map(_.getId).toSet

      val mentions = getMentions(id, username, replyIds ++ retweetIds)

      mentions ++ replies.filter { t =>
        val replyId = t.getInReplyToStatusId
        replyId == id || retweetIds.contains(replyId) || replyIds.contains(replyId)
      }
    }

    private def getQuotingTweets(id: Long, username: String): List[Status] = {
      querySince(s"url:twitter.com/$username/status/$id", id).filter(_.getQuotedStatusId == id)
    }

    def getResponses(id: Long): List[Status] = {
      val username: String = t.tweets.showStatus(id).getUser.getScreenName
      getReplies(id, username) ++ getQuotingTweets(id, username)
    }
  }
}

object ResponseAnalysis {

  def loadHelperText(): Set[String] = {
    scala.io.Source.fromFile("src/main/resources/ignore_words.txt").getLines().toList.toSet
  }

  def responseWordCount(responses: List[Status]): List[(String, Int)] = {
    val ignore = loadHelperText()

    val words: List[String] = responses
      .map(_.getText)
      .map(_.toLowerCase)
      .flatMap(_.split(' '))
      .filterNot(_.startsWith("@"))
      .map(_.replaceAll("[^A-Za-z]", ""))
      .filterNot(word => ignore(word))

    val reduced = words.foldLeft(Map.empty[String, Int]) { (counts, word) =>
      val count = counts.getOrElse(word, 0) + 1
      counts + (word -> count)
    }

    reduced.toList.sortBy(_._2).reverse
  }

  def sortByPopularity(responses: List[Status]): List[Status] = {
    val popularityOrdering = Ordering.by { status: Status => status.getFavoriteCount + status.getRetweetCount }
    responses.sorted(popularityOrdering).reverse
  }

  def displayResponses(responses: List[Status]): Unit = {
    println(s"\nFound ${responses.size} reponses to your question!")
    println(s"\nHere are some of the most popular responses:")

    val topPopular = sortByPopularity(responses).take(5).foreach { r =>
      println(s"\n@${r.getUser.getScreenName} said:")
      println(r.getText)
    }

    val wc = responseWordCount(responses)

    println("\nHere's a breakdown of the word count from the responses:")
    wc.take(50).foreach(println)
  }

  def main(args: Array[String]): Unit = {
    import TwitterClient._

    val twitter: Twitter = TwitterClient()
    val whyScala: Long = 820426791433039872L
    val status: Long = args.headOption.flatMap(arg => Try(arg.toLong).toOption).getOrElse(whyScala)

    val responses = twitter.getResponses(status)

    displayResponses(responses)
  }

}
