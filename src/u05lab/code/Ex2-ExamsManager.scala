package u05lab.code

object ResultKind extends Enumeration {
  type Kind = Value
  val FAILED, RETIRED, SUCCEEDED = Value
}

case class ExamResult(kind: ResultKind.Kind, evaluation: Option[Int] = None, cumLaude: Boolean = false) {
  if(!evaluation.isEmpty && evaluation.get > 30) throw new IllegalArgumentException("The evaluation must be less or equal then 30.")
  if(!evaluation.isEmpty && evaluation.get < 18) throw new IllegalArgumentException("The evaluation must be greater or equal then 18.")

  override def toString: String = kind match {
    case ResultKind.SUCCEEDED if cumLaude => kind.toString + "(30L)"
    case ResultKind.SUCCEEDED => kind.toString + "(" + evaluation.get + ")"
    case _ => kind.toString
  }
}

sealed trait ExamsResult {
  def failed: ExamResult
  def retired: ExamResult
  def succeededCumLaude: ExamResult
  def succeeded(vote: Int): ExamResult
}

object ExamsResult {
  def apply():ExamsResult = ExamsResultImpl()

  private case class ExamsResultImpl() extends ExamsResult {
    override def failed: ExamResult = ExamResult(ResultKind.FAILED)
    override def retired: ExamResult = ExamResult(ResultKind.RETIRED)
    override def succeededCumLaude: ExamResult = ExamResult(ResultKind.SUCCEEDED, Some(30), true)
    override def succeeded(vote: Int): ExamResult = ExamResult(ResultKind.SUCCEEDED, Some(vote))
  }
}

sealed trait ExamsManager {
  def createNewCall(call: String)
  def addStudentResult(call: String, student:String, result: ExamResult)
  def getAllStudentsFromCall(call: String):Set[String]
  def getEvaluationsMapFromCall(call: String):Map[String, Int]
  def getResultsMapFromStudent(call: String):Map[String, ExamResult]
  def getBestResultFromStudent(call: String):Option[Int]
}

object ExamsManager {
  def apply():ExamsManager = ExamsManagerImpl()

  private case class ExamsManagerImpl() extends ExamsManager {

    private def callNotExist = throw new IllegalArgumentException("Call does not exits")
    private var items:Map[String, Map[String, ExamResult]] = Map()

    override def createNewCall(call: String) = items.contains(call) match {
      case true => throw new IllegalArgumentException("Call " + call + " already exits")
      case _ => items += (call -> Map())
    }

    override def addStudentResult(call: String, student:String, result: ExamResult) = items.get(call) match {
      case None => callNotExist //
      case _ if items(call).contains(student) => throw new IllegalArgumentException("Student already exits in call: " + call)
      case _ =>  items += (call -> (items(call) + (student -> result)))
    }

    override def getAllStudentsFromCall(call: String):Set[String] = items.getOrElse(call, callNotExist).keySet

    override def getEvaluationsMapFromCall(call: String):Map[String, Int] =
      items
        .getOrElse(call, callNotExist)
        .collect({
          case (student, result) if result.kind == ResultKind.SUCCEEDED => student -> result.evaluation.get
        })

    override def getResultsMapFromStudent(student: String):Map[String, ExamResult] =
      items
        .collect({
          case (call, results) if results.contains(student) => call -> results(student)
        })

    override def getBestResultFromStudent(student: String):Option[Int] =
      items
        .collect({
          case (call, results) if results.contains(student) && results(student).kind == ResultKind.SUCCEEDED => (call -> results(student).evaluation.get)
        })
        .valuesIterator
        .reduceLeftOption(_ max _)
  }
}

object ExamsManagerTest extends App {
  /* See: https://bitbucket.org/mviroli/oop2018-esami/src/master/a01b/e1/Test.java */
}