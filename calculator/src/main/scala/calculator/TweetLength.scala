package calculator

object TweetLength {
  final val MaxTweetLength = 140
  val len = new Var[Int](0)
  val color = new Var[String]("green")

  def tweetRemainingCharsCount(tweetText: Signal[String]): Signal[Int] = {
    len() = MaxTweetLength - tweetLength(tweetText());
    len
  }

  def colorForRemainingCharsCount(remainingCharsCount: Signal[Int]): Signal[String] = {
    color() = getColor(remainingCharsCount())
    color
  }

  private def getColor(len:Int) = {
    if (len >= 15) "green"
    else if (len >= 0 && len < 15) "orange"
         else "red"
  }

  /** Computes the length of a tweet, given its text string.
   *  This is not equivalent to text.length, as tweet lengths count the number
   *  of Unicode *code points* in the string.
   *  Note that this is still a simplified view of the reality. Full details
   *  can be found at
   *  https://dev.twitter.com/overview/api/counting-characters
   */
  private def tweetLength(text: String): Int = {
    /* This should be simply text.codePointCount(0, text.length), but it
     * is not implemented in Scala.js 0.6.2.
     */
    if (text.isEmpty) 0
    else {
      text.length - text.init.zip(text.tail).count(
          (Character.isSurrogatePair _).tupled)
    }
  }
}
