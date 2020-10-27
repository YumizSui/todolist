package utility

/**
  * 文字列のダイジェスト化を行う
  */
object Digest {

  /**
    * デフォルトアルゴリズム
    */
  val DEFAULT_DIGEST_ALGORITHM = "SHA-256"

  /**
    * messageをダイジェスト化した文字列を返す関数
    * @param message ダイジェスト化したい文字列
    * @param salt    SALT文字列（default: "salt"）
    * @return        message + salt をダイジェスト化した文字列
    */
  def apply(message: String, salt: String = "salt"): String = {
    val messageDigest = java.security.MessageDigest.getInstance(DEFAULT_DIGEST_ALGORITHM)
    val bytes         = s"${message}${salt}".getBytes()
    messageDigest.update(bytes, 0, bytes.length)
    bytesToHexString(messageDigest.digest())
  }

  private def bytesToHexString(bytes: Array[Byte]): String =
    bytes
      .map(_ & 0xff)
      .map(_.toHexString)
      .map {
        case hexStr if hexStr.length() == 1 => s"0${hexStr}"
        case hexStr                         => hexStr
      }
      .mkString("")
}
