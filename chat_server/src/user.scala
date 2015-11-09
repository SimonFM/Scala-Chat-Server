/**
 * Created by simon on 07/11/2015.
 */
class user {
  private var name = "";
  private var password = ""

  def setUsername(n:String): Unit ={
    name = n;
  }

  def getUsername() : String = {
    if(name != "") return name;
    else return "EMPTY";
  }

  def setPassword(p:String): Unit ={
    password = p;
  }

  def getPassword() : String ={
    if(password != "") return password;
    else return "EMPTY";
  }
}
