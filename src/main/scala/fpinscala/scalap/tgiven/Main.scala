package fpinscala.scalap.tgiven

/**
  * @date 2023-02-07 9:17
  * @author chenzhr
  * @Description
  */
object Main :
  class PreferredPrompt(val preference: String)

  object Greeter:
    def greet(name: String)(using prompt: PreferredPrompt) =
      println(s"Welcome, $name. The system is ready.")
      println(prompt.preference)
  end Greeter

  object JillsPrefs:
    given prompt: PreferredPrompt = PreferredPrompt("Your wish> ")

  end JillsPrefs

  def main(args: Array[String]): Unit =
    // 通过引入JillsPrefs.prompt, 编译器能够隐式的提供Greeter.greet需要的PreferredPrompt
    import JillsPrefs.prompt
    Greeter.greet("kaka!")

    // 被声明为上下文参数(given)后, 不能已常规方式传参, 程序无法通过编译
//    Greeter.greet("kaka")(JillsPrefs.prompt)
    // 必须在调用点显式的使用using关键字来表明填充的是上下文参数
    Greeter.greet("kaka")(using JillsPrefs.prompt)

end Main

