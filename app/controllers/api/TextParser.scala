package controllers.api

import edu.stanford.nlp.ling.CoreLabel
import play.api.libs.json.Json
import play.api.mvc._
import utils.ITextParser

case class ToSnTn(token : CoreLabel, context : String, sentenceNo : Int, tokenNo : Int)
case class NerWordSnTn(ner : String, word : String, context : String, sentenceNo : Int, tokenNo : Int)

object TextParser extends Controller with ITextParser {

  def parse_text_action() = Action { implicit request =>
//    val doc:Annotation = parse_text(text)
//    val json = convert_document(doc)
    println(request.body.asText)
    val doc= parse_text(request.body.asText.getOrElse(""))

    val tokensAndSentNo = sentences(doc).flatMap(x=> tokens(x._1).map(t=>ToSnTn(t._1,"" + x._2 + ":" +  x._1.toString, x._2,t._2)))
    val nerMap = tokensAndSentNo.map(y=> NerWordSnTn(ner(y.token),word(y.token),y.context,  y.sentenceNo, y.tokenNo)).filter{t=>
      !(t.ner.toString.equals("O"))
    }
    val onlyMin = nerMap.filter(x=> x.tokenNo == nerMap.filter(z=>(z.sentenceNo==x.sentenceNo) && (z.ner == x.ner)).map(t=>t.tokenNo).min)

    val sent = sentences(doc).map(x=> NerWordSnTn("SENTIMENT", sentiment(x._1), x.toString(),x._2,0))
    println(sent)

    val json = (onlyMin++sent).map(x=>Json.obj("ner"->x.ner, "word"-> x.word, "context"->x.context, "index" -> x.sentenceNo ))
    println(json)

    Ok(Json.toJson(json))
  }

  def index = Action {
    Ok(views.html.index("MY TEXT"))
  }


}

//object test extends App with utils.TextParserJson {
//  val doc= parse_text("today I am king. david was sick. I love david. I will go to Israel")
//
//  val tokensAndSentNo = sentences(doc).flatMap(x=> tokens(x._1).map(t=>ToSnTn(t._1,x._1.toString, x._2,t._2)))
//  val nerMap = tokensAndSentNo.map(y=> NerWordSnTn(ner(y.token),word(y.token), y.context, y.sentenceNo, y.tokenNo)).filter{t=>
//    !(t.ner.toString.equals("O"))
//  }
//  val onlyMin = nerMap.filter(x=> x.tokenNo == nerMap.filter(z=>(z.sentenceNo==x.sentenceNo) && (z.ner == x.ner)).map(t=>t.tokenNo).min)
//
//  val json = onlyMin.map(x=>Json.obj("ner"->x.ner, "word"-> x.word,"context"->x.context,  "index" -> x.sentenceNo ))
//  println(json)
//}