trait Loggable {
  def outLn(any: Any): Unit = {
    println(any)
  }

  def out(any: Any): Unit = {
    print(any)
  }

  def debug(any: Any): Unit = {
    if (false) {
      println(any)
    }
  }
}
