/**
 * This class holds an instance of a simple game where
 * a player moves on a field and collects bounties.
 * See the explanation sheet and comments in this file for details. The constructor builds an
 * instance of a game using the accepted parameters.
 *
 * @param wall     A list of coordinates (as tuples) where walls exist. Example: The parameter List((0,0),(0,1)) puts two wall elements in the upper left corner and the position below.
 * @param bounty   A list of bounties, each is a position and a value (i.e. a 3 value tuple). Example: List((0,0,5)) puts a bounty in the upper left corner which adds 5 to the score.
 * @param initialX The initial x position of the player.
 * @param initialY The initial y position of the player. If initialX and initialY are 0, the player starts in the upper left corner.
 */
class Game(wall: List[(Int, Int)], bounty: List[(Int, Int, Int)], initialX: Int, initialY: Int) {

  //the current grid, a 10x10 field, where -1=empty, 0=wall, any positive number=bounty
  private var field: Array[Array[Int]] = Array.ofDim[Int](10, 10)

  /* Please note - both of the above two-dimensional arrays
   * should be accessed in the format field(col)(row) so field(2)(0) would retrieve the 3rd column and the 1st row (as indexing starts at zero),
   * equivalent to an (x,y) coordinate of (2,0). You may therefore visualise each inner array as representing a column of data.
   */

  //the current score, initially 0
  private var score: Int = 0
  //the current player position. As the player moves these positions update.
  private var positionX: Int = initialX
  private var positionY: Int = initialY
  //the current X and Y save position, initially -1
  private var saveX: Int = -1
  private var saveY: Int = -1

  /* This code is executed as part of the constructor. It firstly initialises all cells to -1 (i.e. empty).
   * It uses the list of walls provided to initialise the walls in the field array by setting given coordinates to 0.
   * It then uses the list of bounties to initialise the bounties in the field array by setting given coordinates to the provided number.
   */
  for (i <- 0 until 10; k <- 0 until 10) field(i)(k) = -1
  wall.foreach(w => field(w._1)(w._2) = 0)
  bounty.foreach(w => field(w._1)(w._2) = w._3)

  /**
   * Repeatedly run a sequence of commands. For example:
   * for(i <- 1 to 5) println("Hello")
   * can be replaced by
   * rpt(5)(println("Hello"))
   */
  def rpt(n: Int)(commands: => Unit): Unit = {
    for (i <- 1 to n) {
      commands
    }
  }

  /** ******************************************************************************
   * Start coding here
   * ***************************************************************** */
  // helper method to check if coordinates are out of the game field
  def isOutOfField(x: Int, y: Int): Boolean = {
    (x < 0 || x > 9) || (y < 0 || y > 9)
  }

  // helper method to check if a wall is at given coordinates
  def isWall(x: Int, y: Int): Boolean = {
    wall.exists(wall => wall._1 == x && wall._2 == y)
  }

  /**
   * Returns the current position of the player as a tuple, in (x,y) order.
   */
  def getPlayerPos(): (Int, Int) = {
    return (positionX, positionY);
  }

  /**
   * Returns the current score.
   */
  def getScore(): Int = score

  /**
   * Move the player one place to the left.
   * If there is a wall or the field ends, nothing happens.
   * If there is a bounty, it is collected (i.e. a call to checkBounty() is made).
   * A more advanced requirement would be to call checkBounties() if completed.
   */
  def arrowLeft(): Unit = {
    // validate if new coordinates are valid
    if (!isOutOfField(positionX - 1, positionY) && !isWall(positionX - 1, positionY)) {
      // change position of player
      positionX -= 1
      // check for bounties after move
      checkBounty()
      checkBounties()
    }
  }

  /**
   * Move the player one place to the right.
   * If there is a wall or the field ends, nothing happens.
   * If there is a bounty, it is collected (i.e. a call to checkBounty() is made).
   * A more advanced requirement would be to call checkBounties() if completed.
   */
  def arrowRight(): Unit = {
    // validate if new coordinates are valid
    if (!isOutOfField(positionX + 1, positionY) && !isWall(positionX + 1, positionY)) {
      // change position of player
      positionX += 1
      // check for bounties after move
      checkBounty()
      checkBounties()
    }
  }

  /**
   * Move the player one place up.
   * If there is a wall or the field ends, nothing happens.
   * If there is a bounty, it is collected (i.e. a call to checkBounty() is made).
   * A more advanced requirement would be to call checkBounties() if completed.
   */
  def arrowUp(): Unit = {
    // validate if new coordinates are valid
    if (!isOutOfField(positionX, positionY - 1) && !isWall(positionX, positionY - 1)) {
      // change position of player
      positionY -= 1
      // check for bounties after move
      checkBounty()
      checkBounties()
    }
  }

  /**
   * Move the player one place down.
   * If there is a wall or the field ends, nothing happens.
   * If there is a bounty, it is collected (i.e. a call to checkBounty() is made).
   * A more advanced requirement would be to call checkBounties() if completed.
   */
  def arrowDown(): Unit = {
    // validate if new coordinates are valid
    if (!isOutOfField(positionX, positionY + 1) && !isWall(positionX, positionY + 1)) {
      // change position of player
      positionY += 1
      // check for bounties after move
      checkBounty()
      checkBounties()
    }
  }

  /**
   * Move the player n places to the left. Negative numbers or 0 as a parameter cause no effect.
   * If there is a wall or the field ends, the player stops before the wall or end of the field.
   * Any bounties are collected (i.e. a call to checkBounty() is made after each move).
   * A more advanced requirement would be to call checkBounties() if completed.
   */
  def arrowLeft(n: Int): Unit = {
    rpt(n) {
      arrowLeft()
    }
  }

  /**
   * Move the player n places to the right. Negative numbers or 0 as a parameter cause no effect.
   * If there is a wall or the field ends, the player stops before the wall or end of the field.
   * Any bounties are collected (i.e. a call to checkBounty() is made after each move).
   * A more advanced requirement would be to call checkBounties() if completed.
   */
  def arrowRight(n: Int): Unit = {
    rpt(n) {
      arrowRight()
    }
  }

  /**
   * Move the player n places up. Negative numbers or 0 as a parameter cause no effect.
   * If there is a wall or the field ends, the player stops before the wall or end of the field.
   * Any bounties are collected (i.e. a call to checkBounty() is made after each move).
   * A more advanced requirement would be to call checkBounties() if completed.
   */
  def arrowUp(n: Int): Unit = {
    rpt(n) {
      arrowUp()
    }
  }

  /**
   * Move the player n places down. Negative numbers or 0 as a parameter cause no effect.
   * If there is a wall or the field ends, the player stops before the wall or end of the field.
   * Any bounties are collected (i.e. a call to checkBounty() is made after each move).
   * A more advanced requirement would be to call checkBounties() if completed.
   */
  def arrowDown(n: Int): Unit = {
    rpt(n) {
      arrowDown()
    }
  }

  /**
   * Checks if the current position is a bounty. A bounty exists if the cell
   * has a value larger than 0. If a bounty does exist, increase the score,
   * and then erase the bounty, i.e. set it to -1.
   */
  def checkBounty(): Unit = {
    checkBounty(positionX, positionY)
  }

  def checkBounty(x: Int, y: Int): Unit = {
    // get the third value from the field list. if the value is greater 0 we found a bounty
    val maybeBounty = field(x)(y)
    if (maybeBounty > 0) {
      // add bounty to our score
      score += maybeBounty
      // set bounty of this field to 0 -> means we already collected it
      field(x)(y) = 0
    }
  }

  def getAllBountyPositions: String = {
    bounty.map { case (x, y, _) => s"($x, $y)" }.mkString(", ")
  }

  //The methods beyond this point (aside to those in GameBuilder which is a separate task) are more complex than those above.

  /**
   * This moves the player according to a string. The string can contain the
   * letters l, r, u, d representing left, right, up, down moves.  If
   * there is a wall or the field ends, the individual move is not
   * executed. Any further moves are done. Any bounties are collected and the
   * save position is evaluated.
   */
  def move(s: String): Unit = {
    // iterate over each character of the move string and call the correct arrowX method
    s.foreach {
      case 'l' => arrowLeft()
      case 'r' => arrowRight()
      case 'u' => arrowUp()
      case 'd' => arrowDown()
    }
  }

  /**
   * Identifies the maximum overall bounty in the game. This is the sum
   * of the current score and the possible score from collecting all of the remaining bounties.
   * No bounties are collected here, only the max score is returned.
   */
  def maxBounty(): Int = {
    var temporaryScore = score
    for (x <- 0 until 10) {
      for (y <- 0 until 10) {
        // iterate over all fields and calculate the max bounty
        val maybeBounty = field(x)(y)
        if (maybeBounty > 0) {
          temporaryScore += maybeBounty
        }
      }
    }
    temporaryScore
  }

  /**
   * Checks if the rectangle defined by the current position and saved position
   * covers nine or more positions. If yes, it collects bounties in it, increases the
   * score, and erases the bounties. Also resets the saved position to -1,-1.
   */
  def checkBounties(): Unit = {
    if (saveX == -1 && saveY == -1) {
      return
    }

    // use the difference between the current position and the saved one an calculate the rectangle size
    val absX = Math.abs(positionX - saveX) + 1
    val absY = Math.abs(positionY - saveY) + 1
    if (absX * absY < 9) {
      return
    }

    // iterate only over the rectangle created by postionX/Y and saveX/Y and collect bounties in this rectangle
    for (x <- saveX to positionX) {
      for (y <- saveY to positionY) {
        checkBounty(x, y)
      }
    }

    // reset position
    setSavePos(-1, -1)
  }

  /**
   * This gives a string in the format for move, which collects the maximum bounty. No specific
   * requirements for the efficiency of the solution exist, but the solution must consist of a finite number
   * of steps. The move is combined of a number of moves
   * given by suggestMove. If these are not possible, an empty string is returned. No bounties are collected
   * and the player must be at the original position after the execution of the method.
   */
  def suggestSolution(): String = {
    // check all bounties first, we store the current position for later
    val originalX = positionX
    val originalY = positionY

    val moves = suggestSolution(bounty, bounty.length)

    positionX = originalX
    positionY = originalY
    moves
  }

  def sortByNearestBounty(b: List[(Int, Int, Int)]): List[(Int, Int, Int)] = {
    // sort all found bounties by distance
    b.sortWith((a, b) => {
      // sum up x + y distance and compare the distance of two bounties
      val distanceA = Math.abs(a._1 - positionX) + Math.abs(a._2 - positionY)
      val distanceB = Math.abs(b._1 - positionX) + Math.abs(b._2 - positionY)
      distanceA < distanceB
    })
  }

  def suggestSolution(currentBounties: List[(Int, Int, Int)], numberOfTries: Int): String = {
    // return an empty string if we have no retries left
    if (numberOfTries == 0) ""
    else {
      var moves = ""
      // sort bounties by nearest to player
      val sortedBounties = sortByNearestBounty(currentBounties)
      // stores bounties for which suggestMove returned an empty string, we will retry them from another position
      var leftoverBounties: List[(Int, Int, Int)] = Nil

      // iterate over all bounties and suggest move
      for (i <- sortedBounties.indices) {
        val b = sortedBounties(i)
        val suggestedMove = suggestMove(b._1, b._2)
        if (suggestedMove.isEmpty) {
          moves = ""
          leftoverBounties = b :: leftoverBounties
        } else {
          // add move string to moves and set position to bounty position
          moves = moves + suggestedMove
          positionX = b._1
          positionY = b._2
        }
      }
      // try leftover bounties
      moves + suggestSolution(leftoverBounties, numberOfTries - 1)
    }
  }

  /**
   * This gives a string in the format for move, which moves from the current position to
   * position x,y. No specific requirements for the efficiency of the solution exist. The move
   * cannot jump walls. The method is restricted to finding a path which is combined of a number of
   * left and then a number of up movement, or left/down, or right/up, or right/down movements only.
   * If this is not possible due to walls, it returns an empty string. No actual move is done. If
   * x or y are outside the field, an empty string is returned as well.
   */
  def suggestMove(x: Int, y: Int): String = {
    // should we go left or right
    val xCharacter = if (x > positionX) "r" else "l"
    // should we go up or down
    val yCharacter = if (y > positionY) "d" else "u"

    val (lowerY, upperY) = if (positionY > y) (y, positionY) else (positionY, y)
    val (lowerX, upperX) = if (positionX > x) (x, positionX) else (positionX, x)

    // try to move from lowerY to upperY
    val (moveY, hitWallOrOutOfFieldY) = testYAxis(positionX, lowerY, upperY, yCharacter)
    if (!hitWallOrOutOfFieldY) {
      // try to move from lowerX to upperX
      val (moveX, hitWallOrOutOfField) = testXAxis(y, lowerX, upperX, xCharacter)
      if (!hitWallOrOutOfField) {
        // if we do not hit a wall or are out of field this is a valid move
        return moveY + moveX
      }
    }

    // try to move from lowerX to upperX
    val (moveX, hitWallOrOutOfFieldX) = testXAxis(positionY, lowerX, upperX, xCharacter)
    if (!hitWallOrOutOfFieldX) {
      // try to move from lowerY to upperY
      val (moveY, hitWallOrOutOfField) = testYAxis(x, lowerY, upperY, yCharacter)
      if (!hitWallOrOutOfField) {
        // if we do not hit a wall or are out of field this is a valid move
        return moveX + moveY
      }
    }

    // return an empty string if no move can be suggested
    ""
  }

  // helper method which checks if we can move from lowerY to upperY without hitting a wall or move out of field
  def testYAxis(x: Int, lowerY: Int, upperY: Int, yCharacter: String): (String, Boolean) = {
    var move = ""
    for (currentY <- lowerY until upperY) {
      if (!isOutOfField(x, currentY) && !isWall(x, currentY)) {
        move += yCharacter
      } else {
        return ("", true)
      }
    }
    (move, false)
  }

  // helper method which checks if we can move from lowerX to upperX without hitting a wall or move out of field
  def testXAxis(y: Int, lowerX: Int, upperX: Int, xCharacter: String): (String, Boolean) = {
    var move = ""
    for (currentX <- lowerX until upperX) {
      if (!isOutOfField(currentX, y) && !isWall(currentX, y)) {
        move += xCharacter
      } else {
        return ("", true)
      }
    }
    (move, false)
  }

  /* --- The three save methods below are used by the unit tests to simulate certain conditions --- */

  /**
   * Updates saveX and saveY to the current player position.
   */
  def save(): Unit = {
    /* This method is already implemented. You should not change it */
    saveX = positionX
    saveY = positionY
  }

  /**
   * Returns the current save position as a tuple, in (x,y) order.
   */
  def getSavePos(): (Int, Int) = {
    /* This method is already implemented. You should not change it */
    return (saveX, saveY);
  }

  /**
   * Sets the savePos to the values of the parameters.
   */
  def setSavePos(saveX: Int, saveY: Int): Unit = {
    /* This method is already implemented. You should not change it */
    this.saveX = saveX
    this.saveY = saveY
  }

  def areAllBountiesCollected: Boolean = {
    bounty.forall { case (x, y, value) => field(x)(y) != value }
  }

}

/**
 * This object builds and returns a standard instance of Game.
 * It is used by the unit tests to initialise the game in different states.
 * Currently, there are three ways in which a game can be initialised,
 * the first has been completed but the other two initialisation methods need writing.
 */
object GameBuilder {
    /**
     * @return A game with
     *         - walls in positions 3,0 3,1 and 3,2
     *         - a bounty at 4,1 which increases score by 5
     *         - a bounty at 3,3 which increases score by 10
     *         - the player starting in position 0,0
     */
    def initialiseGame1(): Game = {
      /* This method is already implemented. You should not change it */
      return new Game(List((3, 0), (3, 1), (3, 2)), List((4, 1, 5), (3, 3, 10)), 0, 0)
    }

    /**
     * @return A game with
     *         - walls in positions 3,3 3,4 3,5 5,3 5,4 and 5,5
     *         - a bounty at 4,4 which increases score by 1
     *         - a bounty at 6,3 which increases score by 1
     *         - the player starting in position 3,2
     */
    def initialiseGame2(): Game = {
      List((4, 4, 1), (6, 3, 1))
      return new Game(List((3, 3), (3, 4), (3, 5), (5, 3), (5, 4), (5, 5)), List((4, 4, 1), (6, 3, 1)), 3, 2)
    }

    /**
     * @return A game with
     *         - walls in positions 3,0 3,1 and 3,2
     *         - a bounty at 4,1 which increases score by 5
     *         - a bounty at 3,3 which increases score by 10
     *         - the player starting in position 4,1
     */
    def initialiseGame3(): Game = {

      return new Game(List((3, 0), (3, 1), (3, 2)), List((4, 1, 5), (3, 3, 10)), 4, 1)
    }

}
object GameRunner {
  //def main(args: Array[String]): Unit = {
    // Initialize the game with some default values or ask the user for input
    val game = new Game(
      wall = List((3, 0), (3, 1), (3, 2)), // Example walls
      bounty = List((4, 1, 5), (3, 3, 10)), // Example bounties
      initialX = 0,
      initialY = 0
    )

    var input = ""
    val scanner = new java.util.Scanner(System.in)

    println("Welcome to the Game!")
    printInstructions()

    while (input != "quit" && !game.areAllBountiesCollected) {
      print("Enter command: ")
      input = scanner.nextLine()

      input match {
        case "left" => game.arrowLeft()
        case "right" => game.arrowRight()
        case "up" => game.arrowUp()
        case "down" => game.arrowDown()
        case "print" => println(s"Player Position: ${game.getPlayerPos()}")
        case "score" => println(s"Score: ${game.getScore()}")
        case "save" => game.save()
        case "load" => game.setSavePos(game.getSavePos()._1, game.getSavePos()._2)
        case _ => printInstructions()
      }
      if (game.areAllBountiesCollected) {
        println(s"Congratulations! All bounties collected")
      } else {
        println("Game ended by user.")
        // Display all bounty positions when the game is quit
        println(s"All bounty positions: ${game.getAllBountyPositions}")
      }

      // Display current state (optional)
      println(s"Position: ${game.getPlayerPos()}, Score: ${game.getScore()}")
    }
  }

  def printInstructions(): Unit = {
    println("Enter one of the following commands to play the game:")
    println("'left' - move left")
    println("'right' - move right")
    println("'up' - move up")
    println("'down' - move down")
    println("'print' - print current position")
    println("'score' - print current score")
    println("'save' - save current position")
    println("'load' - load saved position")
    println("'quit' - exit the game")
 //}
}