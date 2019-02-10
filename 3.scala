import breeze.linalg._
import scalalab.JavaUtilities
import breeze.linalg.{max, sum}
object PSO {

  def main(args: Array[String]): Unit = {
    import java.util.concurrent.ThreadLocalRandom
    import breeze.math.Complex.scalar
    import scala.collection.mutable.ArrayBuffer


    var problem = new Problem()

    def CostFunction(x : DenseVector[Double]) =null

    
    println("***Enter values for follwoing arguments***")
    println()
    print("Number of Dimensions(d) = ")
    var nvar = scala.io.StdIn.readInt(); //Dimensions ; Number of unknown Decision variables
    println("Number of Dimensions(d) = " + nvar)
    var VarSize = DenseVector[Double](nvar) //Matrix containing solution

    var MinVal = -512; //Lower Bound of Decision variables
    var MaxVal = 512 //Upper Bound of Decision variables

 

    print("Number of Maximum Iterations = ")
    var MaxIt = scala.io.StdIn.readInt() //Maximum number of iteration
    println("Number of Maximum Iterations = " + MaxIt)

    print("Population (SWARM) Size = ")
    var nPop = scala.io.StdIn.readInt() //Population Size (Swarm Size)
    println("Population (SWARM) Size = " + nPop)
    println()
    println("Press any key to continue")

    scala.io.StdIn.readLine()

    var w: Double = chi //Inertia
    val c1 = 1.34
    val c2 = 1.34
    val wdump = 1

    var MaxVelocity:Double = 0.2 * (MaxVal - MinVal)
    var MinVelocity:Double = -MaxVelocity


    val random: ThreadLocalRandom = ThreadLocalRandom.current();
    var particle = new ArrayBuffer[empty_Particle](nPop) 

    var GlobalBest_Cost = Double.PositiveInfinity
    var GlobalBest_Position = DenseVector.fill(nvar)(math.random)

    for (i <- 0 to nPop) {
      particle.append(new empty_Particle(nvar))
      particle(i).Position = DenseVector.fill(nvar)(random.nextDouble(MinVal, MaxVal + 1)) //Array.fill(nvar) (random.nextDouble(MinVal , MaxVal + 1))
      particle(i).Cost =problem.Sphere(particle(i).Position)
      particle(i).Velocity = DenseVector.fill(nvar)(0.0)

      particle(i).BestPosition = particle(i).Position
      particle(i).BestCost = particle(i).Cost

      if (particle(i).BestCost < GlobalBest_Cost) {
        GlobalBest_Cost = particle(i).BestCost
        GlobalBest_Position = particle(i).BestPosition
      }
    }


    for (iteration: Int <- 0 to MaxIt-1) // for all iteration
    {
      for (i <- 0 to nPop-1) 
      {
        
        particle(i).Velocity = (w * particle(i).Velocity) +
          c1 * DenseVector.fill(nvar)(math.random) .* (particle(i).BestPosition - particle(i).Position) +
          c2 * DenseVector.fill(nvar)(math.random) .* (GlobalBest_Position - particle(i).Position)

        particle(i).Velocity = max(particle(i).Velocity, MinVelocity)
        particle(i).Velocity = min(particle(i).Velocity, MaxVelocity)


      
        particle(i).Position = particle(i).Position + particle(i).Velocity

       
        particle(i).Position = max( particle(i).Position,MinVal.toDouble) 
        particle(i).Position = min( particle(i).Position,MaxVal.toDouble) 

        particle(i).Cost = problem.Sphere(particle(i).Position)

   
        if (particle(i).Cost < particle(i).BestCost) //check for best cost and position till now gained by particle
        {
          particle(i).BestPosition = particle(i).Position;
          particle(i).BestCost = particle(i).Cost


          if (particle(i).BestCost < GlobalBest_Cost) {
            GlobalBest_Cost = particle(i).BestCost
            GlobalBest_Position = particle(i).BestPosition
          }
        }
      }

      println("Best Cost of Iteration:" + iteration + " is= " + GlobalBest_Cost)
      w = w * wdump
    }
    println("Global Best Cost :" + GlobalBest_Cost)
    println("Global Best Position :" + GlobalBest_Position)

    }


  }

}