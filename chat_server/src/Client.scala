/**
 * Created by Simon on 05/11/2015.
 */

import java.io._
import java.net.{Socket}
import java.util
import java.util.concurrent.{Executors}
import scala.io._

class Client {
  // user details
  var username = ""
  var password = ""

  // server details
  val ip = "localhost"
  val port = 8080

  var socket: Socket = null
  var outputStream: OutputStream = null
  var in:  Iterator[String] = null
  var out: PrintWriter = null
  var fistTime = true
  var notConverted = true

  def connect(): Unit ={
    socket = new Socket(ip,port)
    outputStream = socket.getOutputStream()
    in = new BufferedSource(socket.getInputStream()).getLines()
    out = new PrintWriter(new BufferedWriter(new OutputStreamWriter(outputStream,"UTF-8")))
    username = readLine("Please enter username: ")
    password = readLine("Please enter password: ")
    out.println("JOIN_CHATROOM: "+username+" PASSWORD: "+password)
    out.flush()
  }

  def run(): Unit ={
    try{
      connect()
      var message = ""
      //Loop forever, while the socket isn't disconnected
      while(!socket.isClosed){
        if(message == "KILL_SERVICE"){
          out.println(message)
          out.flush()// send the request to the server
        }
        else{
          if(in.hasNext){
            var recv = in.next()
            if(notConverted){
              if(recv == "KILL_SERVICE\n"){
                notConverted = false
                println("Socket Closed!")
                socket.close()
              }
              else{
                handleMessage(recv.split(" "))
                // A check to see if the input isn't Empty
                // Otherwise tell it to stop
                if(in.hasNext) recv = in.next()
                else notConverted = false
              }
            }
          }
        }
        message = readLine("Please enter message: ")
        out.println("MESSAGE: "+username+" "+message)
        out.flush()// send the request to the server
      }
    }catch{
      case notConnected : java.net.ConnectException =>
        println("Can't connect to: "+ip+" on port: "+port)
    }

  }

  def handleMessage(message:Array[String]): Unit ={
    message(0) match{
      case "MESSAGE:" =>
        println(message.mkString(" ") drop 9)
      case "JOIN_CHATROOM:" =>
        println(message.mkString(" ") drop 15 )
      case "KILL_SERVICE\n" =>
        notConverted = false
        println("Socket Closed!")
        socket.close()
      case default => println(message.mkString(" ") )
   }
  }

}
