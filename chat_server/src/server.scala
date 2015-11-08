/**
 * Created by Simon on 10/10/2015.
 *
 * I found this tutorial helpful in figuring out how to use
 * thread pools and concurrency in scala, I do take credit this implementation.
 * The library is built in to java as you can see, so all that work is
 * handled by that.
 *
 * https://twitter.github.io/scala_school/concurrency.html
 */
import java.io._
import java.net._
import java.util.concurrent.{Executors}
import scala.io._

object server{
  /**
   * This is a simple server class to represent a multi threaded server.
   * It contains both a Server and a Worker class. The worker doing all the
   * work for the server.
   * @param portNumber - The port the server operates on.
   */


  class Server(portNumber: Int) extends Runnable {
    var NUMBER_OF_THREADS = 20 // Maximum number of threads.
    val serverSocket = new ServerSocket(portNumber) // setting up the server
    println("Server running on port number: " + portNumber) // display to console
    val threadPool = Executors.newFixedThreadPool(NUMBER_OF_THREADS) // create the thread pool
    var sockets: List[Socket] = List()
    var messages: List[String] = List()

    val admin = new user
    admin.setPassword("a")
    admin.setUsername("admin")

    val test = new user
    test.setPassword("a")
    test.setUsername("test")

    var users: List[user] = List(admin,test)
    var loggedIn: List[user] = List()
    var recv = "" // variable to store messages.
    /**
     * This is the run method of the server, it is needed as I have extended my server
     * to be Runnable, so I could have multiple servers should the need arise.
     * It creates a new socket for every new connection to the server.
     * It loops forever, as long as the server is not closed.
     */
    def run(): Unit = {
      try {
        while (!serverSocket.isClosed) {
          try{
            sockets = serverSocket.accept() :: sockets
            if (!sockets.isEmpty){
              println("Made a new worker")
              threadPool.execute(new Worker(sockets.head)) // allocate a new Worker a Socket
            }
            else println("Empty socket list")
          }catch {
            case socketE: SocketException => println("Sorry, the server isn't running")
          }
        }
      } finally {
        println("Thread Pool shutdown")
        threadPool.shutdown()
      }
    }

    /**
     * A class that handles the work for the server. It takes in a connection
     * from the server and does some work based off of input to the socket.
     */
    class Worker(socket: Socket) extends Runnable {

      // generic socket set up. ( used from the last lab)
      lazy val in = new BufferedSource(socket.getInputStream()).getLines()


      /**
       * This is where the work of the worker is done, it checks the
       * message for either KILL_SERVICE or "HELO " as does tasks depending
       * on the input
       * - Replying with the desired string if HELO
       * - Or it kills the server if KILL_SERVICE
       */
      def run() {
        var myUser = ""
        try {
          println(sockets.size)
          // if there is another message, get it.
          while(!socket.isClosed){
            if (in.hasNext) {
              recv = in.next()
              println("Received: " + recv)
            }
            if(recv != ""){
              val temp = recv.split(" ")
              temp(0) match {
                // handle the kill service protocol
                case "KILL_SERVICE" =>
                  handleMessage("KILL_SERVICE")
                  // tell the client the server shut down
                  shutdownServer() // call the shut down method
                  socket.close() // close the socket (ie the thread).
                //handle joining a chat room
                case "JOIN_CHATROOM:" =>
                  if(login(temp(1),temp(3))){
                    myUser = temp(1)
                    for (s <- sockets) {

                      val outputStream = s.getOutputStream()
                      val out = new PrintWriter(new BufferedWriter(new OutputStreamWriter(outputStream, "UTF-8")))
                      if(temp.length == 4) {
                        messages = (temp(0)+" '"+temp(1)+"' has logged in") :: messages
                        out.println(temp(0)+" '"+temp(1)+"' has logged in")
                        out.flush()
                      }
                      else{
                        val outputStream = socket.getOutputStream()
                        val out = new PrintWriter(new BufferedWriter(new OutputStreamWriter(outputStream, "UTF-8")))
                        out.println("ERROR - 1")
                        out.flush()
                      }
                    }
                  }
                  else{
                    val outputStream = socket.getOutputStream()
                    val out = new PrintWriter(new BufferedWriter(new OutputStreamWriter(outputStream, "UTF-8")))
                    out.println("Sorry no such user or invalid password")
                    out.println("KILL_SERVICE")
                    out.flush()
                  }
                //handle new message
                case "MESSAGE:" =>
                  println("New Message")
                  handleMessage(temp(0) + " " + temp(1) + " says: " + temp(2))
                //handle leaving a chat room
                case "LEAVE_CHATROOM:" =>
                  println("Leave ChatRoom")
                  handleMessage("LEAVE_CHATROOM")
                //gibberish
                case default => println("????")
              }//end of switch
            }
            handleMessage("IM_STILL_HERE:")
          }// end of while
        } catch {
          case s: SocketException => println("User "+myUser+" the plug")
        }
      }
    }


    def handleMessage(message:String): Unit ={
      messages = recv :: messages
      for (s <- sockets) {
        val outputStream = s.getOutputStream()
        val out = new PrintWriter(new BufferedWriter(new OutputStreamWriter(outputStream, "UTF-8")))
        out.println(message)
        out.flush()
      }
    }
    /**
     * This function kills the server.
     */
    def shutdownServer(): Unit = {
      try{
        if(serverSocket != null) {
          serverSocket.close()
          threadPool.shutdownNow()
          println("Server shut down")
        }
      }catch{
        case e: SocketException => println("Server shut down")

      }
    }

    /**
     * A fucntion that handles login
     * @param name, password
     */
    def login(name:String,pass:String): Boolean ={
      for(u <- users){
        if(u.getUsername() == name && u.getPassword() == pass){
          loggedIn = u :: loggedIn
          return true
        }
      }
      return false
    }

  }



  def main(args: Array[String]){
    try{
      new Server(args(0).toInt).run()
    }catch{
      case outOfBounds : java.lang.ArrayIndexOutOfBoundsException =>
        println("Please provide command line arguments")
    }
  }
}



