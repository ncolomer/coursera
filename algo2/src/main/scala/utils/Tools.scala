package utils

import scala.io.Source

object Tools {

  def loadTextFile(file: String): Iterator[String] =
    Source.fromURL(getClass.getClassLoader.getResource(file)).getLines

}
