/**
 * Created by Simon on 10/10/2015.
 * Based off my previous lab
 */
import java.io._
import java.net._
import java.util.concurrent.Executors

object server{

  var clientPorts = 8000
  var clientID = 0;


  // Class that handles a user
  class User {
    private var name = "";
    private var password = ""
    private var socket = null
    private var id = 0

    def setUsername(n:String): Unit = {name = n;}

    def getUsername() : String = {
      if(name != "") return name;
      else return "EMPTY";
    }

    def setJoinID(n:Int) : Unit = {id = n}

    def getJoinID() : Int = {return id}

    def setPassword(p:String): Unit = {password = p;}

    def getPassword() : String = {
      if(password != "") return password;
      else return "EMPTY";
    }
  }

  // Class that handles Chat rooms, only deals with sockets so no
  // overhead of dealing with usernames
  class Chatroom(roomName:String, ID:Int){
    val name = roomName
    val roomID = ID
    var users: List[Socket]= List()


    def getRoomName(): String = {return name;}

    def getID(): Int = {return roomID;}

    def addUser(s:Socket):Unit = {users = s :: users}

    def removeUser(s:Socket):Unit ={
      var newList: List[Socket] = List()
      for(u <- users){
        if(u != s) newList = u :: newList
      }
      users = newList
    }

    def forwardMessage(user:String,message:String): Unit ={
      for (s <- users) {
        val outputStream = s.getOutputStream()
        val out = new PrintWriter(new BufferedWriter(new OutputStreamWriter(outputStream, "UTF-8")))
        out.println("CHAT:"+roomID)
        out.println("CLIENT_NAME:"+user)
        out.println("MESSAGE:"+message+"\n")
        out.flush()
      }
    }

    def userDisconnect(usernmae:String,userID:Int): Unit={
      for(s <- users){
        val outputStream = s.getOutputStream()
        val out = new PrintWriter(new BufferedWriter(new OutputStreamWriter(outputStream, "UTF-8")))
        out.println("LEAVE_CHATROOM:"+name)
        out.println("JOIN_ID:"+userID)
        out.println("CLIENT_NAME:"+usernmae+"\n")
        out.flush
      }
    }
  }
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
    var users: List[User] = List()
    var messages: List[String] = List()
    var chatrooms: List[Chatroom] = List()
    var shutdownCalled = false

    var roomID = 0;


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
              println("New Client requested to connect")
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

    def checkRoom(roomName : String): Boolean ={
      for(c <- chatrooms){
        if(c.getRoomName == roomName) return true;
      }
      return false;
    }

    def checkRoomID(roomID : Int): Boolean ={
      for(c <- chatrooms){
        if(c.getID == roomID) return true;
      }
      return false;
    }

    def getChatroom(roomName:String):Chatroom={
      for(c <- chatrooms){
        if(c.getRoomName == roomName) return c;
      }
      return null
    }

    def getChatroomFromID(id:Int):Chatroom={
      for(c <- chatrooms){
        if(c.getID == id) return c;
      }
      return null
    }

    /**
     * A class that handles the work for the server. It takes in a connection
     * from the server and does some work based off of input to the socket.
     */
    class Worker(socket: Socket) extends Runnable {

      // generic socket set up. ( used from the last lab)
      val outputStream = socket.getOutputStream()
      val out = new PrintWriter(new BufferedWriter(new OutputStreamWriter(outputStream, "UTF-8")))
      val inStream = new InputStreamReader(socket.getInputStream)
      lazy val in  = new BufferedReader(inStream)
      var recv = "" // variable to store messages.
      var count = 0

      /**
       * This is where the work of the worker is done, it checks the
       * message for either KILL_SERVICE or "HELO " as does tasks depending
       * on the input
       * - Replying with the desired string if HELO
       * - Or it kills the server if KILL_SERVICE
       */
      def run() {
        try {
          // if there is another message, get it.
          while(!socket.isClosed){
            if(socket.getInputStream().available() > 0){
              println("Waiting....")
              recv = in.readLine()
              println("Received: " + recv)
              if(recv.contains("KILL_SERVICE")) handleKILL
              // If its a HELO message
              else if(recv.contains("HELO ")) handleHELO
              // otherise if its a JOIN_CHATROOM
              else if(recv.contains("JOIN_CHATROOM:")) handleJOIN
              // Handle a message
              else if(recv.contains("CHAT:")) handleCHAT
              //handle leaving:
              else if(recv.contains("LEAVE_CHATROOM:")) handleLeave
              // Otherwise Error
              else{
                handleMessage("ERROR:1")
                handleMessage("This shouldn't have happened")
              }//elsif
            }//if
          //  println("Not gettin any")
          }// end of while


        } catch {
          case s: SocketException => println("User pulled the plug")
        }
      }


      def handleKILL():Unit={
        println(Thread.currentThread.getName() + " is shutting down\n")
        shutdownServer() // call the shut down method
        socket.close() // close the socket (ie the thread).
      }

      def handleHELO(): Unit= {
        val messageWithoutHELO = recv + "\n"
        val ip = socket.getLocalAddress().toString().drop(1) + "\n"
        val port = serverSocket.getLocalPort + "\n"
        handleMessage(messageWithoutHELO + "IP:" + ip + "Port:" + port + "StudentID:ac7ce4082772456e04ad6d80cceff8ddc274a78fd3dc1f28fd05aafdc4665e1b")
      }

      def handleJOIN():Unit={
        println("Got Join Messages")
        var temp = recv.split(":")
        var recvTemp = recv ++"\n"
        var temp1 = ""
        var username: Array[String] = null

        // Collect the rest of the messages for joining
        while(!temp1.contains("CLIENT_NAME:")) {
          temp1 = in.readLine()
          username = temp1.split(":")
          recvTemp = recvTemp ++  temp1
        }
        // server prints out what it gets
        // make a temporary user
        val tempUser = new User
        tempUser.setUsername(username(1))
        tempUser.setJoinID(clientID)
        // add them to the user list, no error checking yet.
        users = tempUser :: users

        // create a temp chatroom
        var chat: Chatroom = null
        var idToSend = 0
        // if there's no chatroom associated with that name, then make it
        // otherwise get that chatroom
        if(!checkRoom(temp(1))) {
          println("Made a new Chatroom")
          chat = new Chatroom(temp(1),roomID)
          chatrooms = chat :: chatrooms
          idToSend = roomID
          roomID = roomID + 1

        }
        else{
          println("Got existing Chatroom")
          chat = getChatroom(temp(1))
          idToSend = chat.getID
        }

        // tell the user they connected
        out.println("JOINED_CHATROOM:"+temp(1))
        out.println("SERVER_IP:"+socket.getLocalAddress().toString().drop(1))
        out.println("PORT:"+serverSocket.getLocalPort)
        out.println("ROOM_REF:"+idToSend)
        out.println("JOIN_ID:"+clientID)
        out.flush()

        // add the user's socket to the chat and forward on the message
        chat.addUser(socket)
        chat.forwardMessage(username(1),username(1) +" has joined this chatroom.")


        clientID = clientID + 1
        clientPorts = clientPorts + 1
        println("Sent Join Messages")
      }

      // For hadnling messages sent to the server
      def handleCHAT():Unit={
        println("Got Chat message")
        var temp = recv.split(":")
        var lines = ""
        lines = recv ++ lines ++ ":"
        //gather the messages
        var temp1 = ""
        var message: Array[String] = null
        while(!temp1.contains("MESSAGE:")) {
          temp1 = in.readLine()
          lines = lines ++ temp1 ++ ":"
          message = temp1.split(":")
        }
        println(lines)
        // create a temp chatroom
        var chat: Chatroom = null
        println(temp(1))
        // if there's no chatroom associated with that name, then make it
        // otherwise get that chatroom
        val id = temp(1).drop(1).toInt
        if(checkRoomID(id)) {
          println("Got existing Chatroom")
          chat = getChatroomFromID(id)
          chat.forwardMessage(temp(1),message(1))
          println("Sent Chat message")
        }

      }

      // Handles the LEAVE_CHATROOM requests
      def handleLeave():Unit={
        println("Got Leave Messages")
        var temp = recv.split(":")
        var recvTemp = recv ++"\n"
        val roomName = temp(1)
        var temp1 = ""
        var username: Array[String] = null

        while(!temp1.contains("CLIENT_NAME:")) {
          temp1 = in.readLine()
          username = temp1.split(":")
          recvTemp = recvTemp ++  temp1 ++"\n"
        }
        val userID = fetchUserID(username(1))
        var chat: Chatroom = null
        if(!checkRoomID(roomName.drop(1).toInt)) println("Sorry not a chatroom")
        else{
          println("Got existing Chatroom")
          chat = getChatroomFromID(roomName.drop(1).toInt)
          out.println("LEFT_CHATROOM:"+roomName)
          out.println("JOIN_ID:"+userID)
          out.flush
          chat.forwardMessage(username(1).drop(1),username(1) +" has left this chatroom.")
          chat.removeUser(socket)
          println("Sent Leave Messages")
        }
      }
    }

    // prints a message to all sockets
    def handleMessage(message:String): Unit ={
      for (s <- sockets) {
        val outputStream = s.getOutputStream()
        val out = new PrintWriter(new BufferedWriter(new OutputStreamWriter(outputStream, "UTF-8")))
        out.println(message)
        out.flush()
      }
    }

    // gets a name from an ID passed
    def fetchUserID(name : String) : Int = {
      for( u <- users){
        if(u.getUsername == name.drop(1)) return u.getJoinID
      }
      return -1
    }
    /**
     * This function kills the server.
     */
    def shutdownServer(): Unit = {
      try{
        if(serverSocket != null) {
          serverSocket.close()
          threadPool.shutdownNow()
          handleMessage("KILL_SERVICE")
          shutdownCalled = true
          System.exit(0)
        }
      }catch{
        case e: SocketException => println("Server shut down")
      }
    }
  }

// starts the server, must be satered with command line parameters for the port
  def main(args: Array[String]){
    try{
      new Server(args(0).toInt).run()
    }catch{
      case outOfBounds : java.lang.ArrayIndexOutOfBoundsException =>
        println("Please provide command line arguments")
    }
  }
}
