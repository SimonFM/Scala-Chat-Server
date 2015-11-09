/**
 * Created by simon on 09/11/2015.
 * This helped me a lot in the GUI aspect of the server
 * http://otfried.org/scala/crossword_example.html
 */

import java.io.{BufferedWriter, OutputStream, OutputStreamWriter, PrintWriter}
import java.net.Socket

import scala.io.BufferedSource
import scala.swing.{BoxPanel, _}
import scala.swing.event.ButtonClicked

class ClientConsole extends Runnable {

  private var username = ""
  private var password = ""
  // server details
  private val ip = "localhost"
  private val port = 8080

  private var socket: Socket = null
  private var outputStream: OutputStream = null
  private var in:  Iterator[String] = null
  private var out: PrintWriter = null
  private var fistTime = true
  private var notConverted = true

  private val messagesLine = new TextArea {
    rows = 10
    lineWrap = true
    wordWrap = true
    editable = false
  }

  class readFromSocket() extends Runnable{
    def run(): Unit = {
      try{
        var message = ""
        //Loop forever, while the socket isn't disconnected
        while(!socket.isClosed){
          println("Im still running")

            var recv = in.next()
            if(notConverted){
              if(recv == "KILL_SERVICE\n"){
                notConverted = false
                println("Socket Closed!")
                socket.close()
              }
              else{
                handleMessage(recv.split(" "))
              }
            }
        }
      }catch{
        case notConnected : java.net.ConnectException =>
          println("Can't connect to: $ip on port: $port")
      }
    }
  }

  def handleMessage(message:Array[String]): Unit ={
    message(0) match{
      case "MESSAGE:" =>
        println(message.mkString(" ") drop 9)
        messagesLine.append(message.mkString(" ") drop 9)
      case "JOIN_CHATROOM:" =>
        println(message.mkString(" ") drop 15 )
        messagesLine.append(message.mkString(" ") drop 15 )
      case "KILL_SERVICE\n" =>
        notConverted = false
        println("Socket Closed!")
        messagesLine.append("Socket Closed!")
        socket.close()
      case default => println(message.mkString(" ") )
    }
  }

  def loginUser(): Unit ={
    socket = new Socket(ip,port)
    outputStream = socket.getOutputStream()
    in = new BufferedSource(socket.getInputStream()).getLines()
    out = new PrintWriter(new BufferedWriter(new OutputStreamWriter(outputStream,"UTF-8")))
    out.println("JOIN_CHATROOM: "+username+" PASSWORD: "+password)
    out.flush()
    var noACK = true;
    var recv = ""
    while(noACK){
      if(in.hasNext){
        recv = in.next()
        println(recv)
        noACK = false
      }
    }

  }

  def sendMessage(message:String): Unit ={
    out.println("MESSAGE: "+username+" "+message)
    out.flush()// send the message to the server
  }



  def run(): Unit = {
    val window = new MainFrame()
    window.preferredSize = new Dimension(500, 500)

    val passwordTextField:TextField = new TextField("a")
    val usernameTextField:TextField = new TextField("test")
    val messageTextField:TextField = new TextField("Enter messages here:")

    val login = new Button("Login")
    val send = new Button("Send")

    val chatLine = new BoxPanel(Orientation.Vertical) {
      contents += Swing.VStrut(1)
      contents += messageTextField
      contents += send
      contents += login
    }



    window.contents = new BoxPanel(Orientation.Vertical) {
      contents += messagesLine
      contents += Swing.VStrut(1)
      contents += new ScrollPane(chatLine)
      border = Swing.EmptyBorder(10, 10, 10, 10)
    }
    window.listenTo(login,send)
    window.reactions += {
      case ButtonClicked(`login`) =>
        println("connecting...")
        username = usernameTextField.text
        password = passwordTextField.text
        loginUser()
        //new Thread(new readFromSocket()).run

      case ButtonClicked(`send`)  =>
        sendMessage(messageTextField.text)
        messageTextField.text = ""
    }
    window.visible = true
  }
}


object runConsole{
  def main(args:Array[String]): Unit ={
    val c = new ClientConsole
    c.run()
  }
}



