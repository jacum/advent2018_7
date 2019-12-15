import scala.io.Source

/**
  * Assumptions:
  *  - focus on readability (typed StepId, case classes preferred over tuples)
  *  - error handling kept at very basic
  *  - performance deemed not critical
  *  - some sanity assertions on sequence validity
  *  - circular dependencies not detected explicitly
  *  - no assertions on ticker logics
  */

object Solution {

  def main(args: Array[String]): Unit = {

    val dataLines = Source.fromResource("data.txt").getLines()
    val rules = parseRules(dataLines)
    val dependencies = extractDependencies(rules)
    val stepSequence = calculateSequence(dependencies)

    println(s"Part 1. Steps: ${ stepSequence.mkString("") }")

    validateSequence(rules, stepSequence)

    val seconds = runSequence(5, stepSequence, dependencies)
    println(s"Part 2. Seconds: $seconds")
  }

  def parseRules(dataLines: Iterator[String]): List[Rule] = {

    val ruleParserRegexp = "Step ([A-Z]) must be finished before step ([A-Z]) can begin".r

    dataLines.map(
      line => {
        val steps = ruleParserRegexp.findAllMatchIn(line).toList.head
        assert(steps.groupCount == 2, s"Line '$line' doesn't match the required format")
        Rule(steps.group(1).charAt(0), steps.group(2).charAt(0))
      }
    ).toList
  }

  type StepId = Char

  case class Rule(
    stepRequired: StepId,
    forStep: StepId
  )

  def extractDependencies(rules: List[Rule]): Map[StepId, List[StepId]] = {
    val stepIds = rules.map(_.stepRequired) ++ rules.map(_.forStep)
    stepIds.map(id =>
      id ->
        rules
          .filter(_.forStep == id)
          .map(_.stepRequired)
          .sorted
    ).sortBy(_._1).toMap
  }

  def calculateSequence(dependencyMap: Map[StepId, List[StepId]]): List[StepId] = {

    def walkMap(stepsDone: List[StepId], stepsRemain: List[StepId]): List[StepId] = {
      stepsRemain match {
        case Nil   => stepsDone
        case steps =>
          val nextStep = steps.filter(possibleStep => {
            dependencyMap(possibleStep).forall(stepsDone.contains)
          }).head
          walkMap(stepsDone ++ List(nextStep), stepsRemain.filterNot(_ == nextStep))
      }
    }

    walkMap(List.empty, dependencyMap.keySet.toList.sorted)
  }

  def validateSequence(rules: List[Rule], steps: List[StepId]): Unit = {

    assert(steps.toSet.size == steps.length, "Not unique")
    assert((rules.map(_.forStep) ++ rules.map(_.stepRequired)).toSet.size == steps.length, "Not all steps included")

    steps.foldLeft("")((stepsDone, step) => {
      assert(rules
        .filter(_.forStep == step)
        .forall(rule => stepsDone.contains(rule.stepRequired)), s"Step $step")
      step + stepsDone
    }
    )
  }

  case class WorkerState(
    task: StepId,
    remaining: Int
  )

  def duration(stepId: StepId): Int = stepId - 'A' + 61

  def runSequence(maxWorkers: Int, sequence: List[StepId], dependencies: Map[StepId, List[StepId]]): Int = {

    def tick(second: Int,
      alreadyDoneTasks: List[StepId],
      remainingTasks: List[StepId],
      workerState: List[WorkerState]): Int = {


      val (justFinished, active) = workerState.partition(_.remaining == 0)

      val completedTasks = alreadyDoneTasks ++ justFinished.map(_.task)

      val freeSlots = maxWorkers - active.length

      val nextWorkerState =
        (if (freeSlots == 0)
          active
        else {
          val readyToDoTasks = remainingTasks.filter(dependencies(_).forall(completedTasks.contains))
          active ++
            readyToDoTasks
              .take(freeSlots)
              .map(t => WorkerState(t, duration(t)))
        }) map {
          case t@WorkerState(_, lastRemaining) => t.copy(remaining = lastRemaining - 1)
        }

      val remainingAfterAssigned = remainingTasks.filterNot(t =>
        nextWorkerState
          .map(_.task)
          .contains(t))
      if (remainingAfterAssigned.isEmpty && nextWorkerState.isEmpty)
        second
      else
        tick(second + 1, completedTasks, remainingAfterAssigned, nextWorkerState)


    }

    tick(0, List.empty, sequence, List.empty)
  }

}
