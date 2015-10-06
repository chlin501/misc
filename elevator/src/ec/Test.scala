package ec

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorLogging
import akka.actor.ActorSystem
import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout
import scala.collection.SortedSet
import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext.Implicits.global

class Proactor extends Actor with ActorLogging {

  protected def unknown: Receive = {
    case msg@_ => log.warning("Unknown message: "+msg)
  }

  protected def name: String = self.path.name

  protected def spawn(name: String, child: Class[_ <: Actor], args: Any*):  
    ActorRef = context.actorOf(Props(child, args: _*), name)

  protected def shutdown = context.system.shutdown

  override def receive = unknown

}

sealed trait ElevatorMsg
case class Init(current: Int) extends ElevatorMsg
case class Go(passenger: Option[Passenger]) extends ElevatorMsg
case object FloorState extends ElevatorMsg
case class Floor(current: Int, goal: Int) extends ElevatorMsg
case class Reached(passenger: Passenger) extends ElevatorMsg

class Elevator(sched: ActorRef, maxFloor: Int) extends Proactor {

  protected var inited = false
  protected var prevPassenger: Option[Passenger] = None
  protected var current = 0
  protected var passengers = Set.empty[Passenger]
  protected var startStep = false

  protected def pickup(): Int = {
    val tmp = passengers.map { passenger => 
      (passenger.goal, Math.abs(currentFloor - passenger.goal)) 
    }
    var min = maxFloor
    tmp.foreach ( e => e._2 < min match { 
      case true => min = e._2
      case false => 
    })
    tmp.find( e => (e._2 == min) ) match {
      case Some(tuple) => tuple._1
      case None => current
    }
  }

  protected def status(): Floor = !passengers.isEmpty match {
    case true => Floor(currentFloor, pickup)
    case false => Floor(currentFloor, currentFloor)
  }

  protected def currentFloor(): Int = this.current
 
  protected def check(passenger: Passenger) = (current == passenger.at) match {
    case true => { startStep = true; step(passenger) }
    case false => startStep match {
      case true => step(passenger) 
      case false => moveTo(passenger)
    }
  }

  protected def moveTo(passenger: Passenger) = current < passenger.at match {
    case true => upTo(passenger)
    case false => downTo(passenger)
  }

  protected def upTo(passenger: Passenger) = if(passenger.at != current) { 
    prevPassenger = Option(passenger)
    current += 1
    log.info("Elevator {} now is UP TO the {}-th floor!", name, 
             currentFloor)
    if(passenger.at == current) startStep = true
    self ! Go(None)
  } else self ! Go(Option(passenger))
  
  protected def downTo(passenger: Passenger) = if(passenger.at != current) {
    prevPassenger = Option(passenger)
    current -= 1
    log.info("Elevator {} now is DOWN TO the {}-th floor!", name, 
             currentFloor)
    if(passenger.at == current) startStep = true
    self ! Go(None)
  } else self ! Go(Option(passenger))

  protected def step(passenger: Passenger) = current < passenger.goal match {
    case true => moveUp(passenger)
    case false => moveDown(passenger)
  }

  protected def moveUp(passenger: Passenger) = if(passenger.goal != current) { 
    prevPassenger = Option(passenger)
    current += 1
    log.info("Elevator {} now is moving UP to the {}-th floor!", name, 
             currentFloor)
    self ! Go(None)
  } else sched ! Reached(passenger)
  
  protected def moveDown(passenger: Passenger) = if(passenger.goal != current) {
    prevPassenger = Option(passenger)
    current -= 1
    log.info("Elevator {} now is moving DOWN to the {}-th floor!", name, 
             currentFloor)
    self ! Go(None)
  } else sched ! Reached(passenger)

  protected def msgs: Receive = {
    case Init(current) => inited match {
      case false => {
        this.current = current
        if(0 >= currentFloor) {
          log.error("Shutdown for current floor {} <= 0!", currentFloor)
          shutdown
        }
        inited = true
        log.info("Elevator {}, having {} floors, now stops at the {}-th floor",
                 name, maxFloor, currentFloor)
      }
      case true => log.warning("Elevator has already been inited!")
    }
    case Go(passenger) => passenger match {
      case Some(p) => {
        passengers += p 
        check(p)  
      }
      case None => prevPassenger match {
        case Some(p) => check(p)
        case None => {
          log.error("Elevator {} should have previous passenger, but None!", 
                    name)
          shutdown
        }
      }
    }
    case FloorState => sender ! status
  }

  override def receive = msgs orElse super.receive
}

case class Initialize(nrOfElevator: Int, nrOfFloor: Int)
case class Passenger(at: Int, goal: Int)

class Scheduler extends Proactor {

  protected var elevators = Set.empty[ActorRef]
  protected var nrOfFloor = 5  
  protected var passengers = Seq.empty[Passenger]
  protected val rand = new scala.util.Random

  protected def random(value: Int): Int = rand.nextInt(value) + 1

  protected def msgs: Receive = {
    case Initialize(n, m) => {
      this.nrOfFloor = if(m > 5) m else 5
      val nn = if(n > 16) 16 else n 
      elevators = (for(idx <- 1 to nn) yield spawn("elevator"+idx, 
        classOf[Elevator], self, nrOfFloor)).toSet
      elevators.foreach( e => e ! Init(random(nrOfFloor)) ) 
      log.info("Initialize {} elevators, each with {} floors!", nn, nrOfFloor)
    }
    case passenger: Passenger => {
      passengers ++= Seq(passenger)
      var map = Map.empty[ActorRef, Int]
      elevators.foreach( e => {
        implicit val timeout = Timeout(60.seconds)
        val f = ask(context.actorSelection(e.path.name), FloorState).
                mapTo[Floor]
        f.onSuccess { case floor: Floor => 
          map += (e -> Math.abs(floor.current - passenger.at))
        }  
      })
      var nearest = Int.MaxValue
      var elevator: Option[ActorRef] = None 
      map.foreach ( e => if(e._2 < nearest) { 
        nearest = e._2
        elevator = Option(e._1)
      })
      log.info("Map contains {}", map)
      log.info("Passenger is at the {}-th floor, going to the {}-th floor. "+
               "Nearest elevator is {}, which has {} steps!", passenger.at,
               passenger.goal, elevator.map { e => e.path.name }.
               getOrElse("UNKNOWN!"), nearest)
      elevator.map { e => e ! Go(Option(passenger)) }  
    }
    case Reached(passenger) => {
      val elevator = sender.path.name
      log.info("[Final] Elevator {} now reaches the {}-th floor!", elevator, passenger.goal)
      // TODO: print and then remove passengers reaches 
    }
  }

  override def receive = msgs orElse super.receive

}

object Test {

  val rand = new scala.util.Random

  def nrfloor(value: Int): Int = rand.nextInt(value) + 1

  def nrelevators(value: Int): Int = rand.nextInt(value) + 1

  def main(args: Array[String]) {
    val sys = ActorSystem("Elevator-Controller-System")
    val sched = sys.actorOf(Props(classOf[Scheduler]), "sched")
    val m = nrfloor(10)
    val n = nrelevators(16)
    sched ! Initialize(m, n)
    Thread.sleep(1000*1)
    println("Passenger at the 3rd floor plans to the 8th floor ...") 
    sched ! Passenger(3, 8)
    Thread.sleep(1000*1)
    println("Passenger at the 6rd floor plans to the 2th floor ...") 
    sched ! Passenger(6, 2)
  }
}
