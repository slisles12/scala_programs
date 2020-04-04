abstract class Role {
  def canAccess(page: String): Boolean
}

class Root extends Role {
  override def canAccess(page: String) = true
}

class SuperAnalyst extends Role {
  override def canAccess(page: String) = page != "Admin"
}

class Analyst extends Role {
  override def canAccess(page: String) = false
}

object Role {
  def apply(roleName: String) = {
    roleName match {
	  case "root" => new Root
	  case "superAnalyst" => new SuperAnalyst
	  case "analyst" => new Analyst
	}
  }
}

object Main {
  def main(args: Array[String]){
    val root = Role("root")  // Role.apply("root")
	val analyst = Role("analyst")
	println(root.canAccess("secret"))
	println(analyst.canAccess("anything"))
	println(Role("superAnalyst").canAccess("Admin"))
  }
}