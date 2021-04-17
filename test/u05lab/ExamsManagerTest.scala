package u05lab.code

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._

class ExamsManagerTest {
  val erf = ExamsResult();
  val em = ExamsManager();

  @Test // verifica base di ExamResultFactory
  def testExamResultsBasicBehaviour() {
    // esame fallito, non c'è voto
    assertEquals(erf.failed.kind, ResultKind.FAILED);
    assertTrue(erf.failed.evaluation.isEmpty);
    assertFalse(erf.failed.cumLaude);
    assertEquals(erf.failed.toString, "FAILED");

    // lo studente si è ritirato, non c'è voto
    assertEquals(erf.retired.kind, ResultKind.RETIRED);
    assertTrue(erf.retired.evaluation.isEmpty);
    assertFalse(erf.retired.cumLaude);
    assertEquals(erf.retired.toString, "RETIRED");

    // 30L
    assertEquals(erf.succeededCumLaude.kind, ResultKind.SUCCEEDED);
    assertEquals(erf.succeededCumLaude.evaluation, Some(30));
    assertTrue(erf.succeededCumLaude.cumLaude);
    assertEquals(erf.succeededCumLaude.toString, "SUCCEEDED(30L)");

    // esame superato, ma non con lode
    assertEquals(erf.succeeded(28).kind, ResultKind.SUCCEEDED);
    assertEquals(erf.succeeded(28).evaluation, Some(28));
    assertFalse(erf.succeeded(28).cumLaude);
    assertEquals(erf.succeeded(28).toString, "SUCCEEDED(28)");
  }

  @Test // verifica eccezioni in ExamResultFactory
  def testExamResultsExcemtionBehaviour() {
    assertThrows(classOf[IllegalArgumentException], () => erf.succeeded(32))
    assertThrows(classOf[IllegalArgumentException], () => erf.succeeded(17))
  }

  // metodo di creazione di una situazione di risultati in 3 appelli
  def prepareExams = {
    em.createNewCall("gennaio");
    em.createNewCall("febbraio");
    em.createNewCall("marzo");

    em.addStudentResult("gennaio", "rossi", erf.failed); // rossi -> fallito
    em.addStudentResult("gennaio", "bianchi", erf.retired); // bianchi -> ritirato
    em.addStudentResult("gennaio", "verdi", erf.succeeded(28)); // verdi -> 28
    em.addStudentResult("gennaio", "neri", erf.succeededCumLaude); // neri -> 30L

    em.addStudentResult("febbraio", "rossi", erf.failed); // etc..
    em.addStudentResult("febbraio", "bianchi", erf.succeeded(20));
    em.addStudentResult("febbraio", "verdi", erf.succeeded(30));

    em.addStudentResult("marzo", "rossi", erf.succeeded(25));
    em.addStudentResult("marzo", "bianchi", erf.succeeded(25));
    em.addStudentResult("marzo", "viola", erf.failed);
  }

  @Test // verifica base della parte obbligatoria di ExamManager
  def testExamsManagement() {
    prepareExams

    // partecipanti agli appelli di gennaio e marzo
    assertEquals(em.getAllStudentsFromCall("gennaio"), Set("rossi","bianchi","verdi","neri"));
    assertEquals(em.getAllStudentsFromCall("marzo"), Set("rossi","bianchi","viola"));

    // promossi di gennaio con voto
    assertEquals(em.getEvaluationsMapFromCall("gennaio").size,2);
    assertEquals(em.getEvaluationsMapFromCall("gennaio").get("verdi"), Some(28));
    assertEquals(em.getEvaluationsMapFromCall("gennaio").get("neri"),Some(30));
    // promossi di febbraio con voto
    assertEquals(em.getEvaluationsMapFromCall("febbraio").size,2);
    assertEquals(em.getEvaluationsMapFromCall("febbraio").get("bianchi"), Some(20));
    assertEquals(em.getEvaluationsMapFromCall("febbraio").get("verdi"), Some(30));

    // tutti i risultati di rossi (attenzione ai toString!!)
    assertEquals(em.getResultsMapFromStudent("rossi").size,3);
    assertEquals(em.getResultsMapFromStudent("rossi").get("gennaio").get.toString,"FAILED");
    assertEquals(em.getResultsMapFromStudent("rossi").get("febbraio").get.toString,"FAILED");
    assertEquals(em.getResultsMapFromStudent("rossi").get("marzo").get.toString,"SUCCEEDED(25)");
    // tutti i risultati di bianchi
    assertEquals(em.getResultsMapFromStudent("bianchi").size,3);
    assertEquals(em.getResultsMapFromStudent("bianchi").get("gennaio").get.toString,"RETIRED");
    assertEquals(em.getResultsMapFromStudent("bianchi").get("febbraio").get.toString,"SUCCEEDED(20)");
    assertEquals(em.getResultsMapFromStudent("bianchi").get("marzo").get.toString,"SUCCEEDED(25)");
    // tutti i risultati di neri
    assertEquals(em.getResultsMapFromStudent("neri").size,1);
    assertEquals(em.getResultsMapFromStudent("neri").get("gennaio").get.toString,"SUCCEEDED(30L)");
  }

  @Test // verifica del metodo ExamManager.getBestResultFromStudent
  def optionalTestExamsManagement() {
    prepareExams

    // miglior voto acquisito da ogni studente, o vuoto..
    assertEquals(em.getBestResultFromStudent("rossi"), Some(25));
    assertEquals(em.getBestResultFromStudent("bianchi"), Some(25));
    assertEquals(em.getBestResultFromStudent("neri"), Some(30));
    assertEquals(em.getBestResultFromStudent("viola"), None);
  }

  @Test
  def optionalTestCantCreateACallTwice() {
    prepareExams
    assertThrows(classOf[IllegalArgumentException], () => em.createNewCall("marzo"))
  }

  @Test
  def optionalTestCantRegisterAnEvaluationTwice() {
    prepareExams
    assertThrows(classOf[IllegalArgumentException], () => em.addStudentResult("gennaio", "verdi", erf.failed))
  }
}
