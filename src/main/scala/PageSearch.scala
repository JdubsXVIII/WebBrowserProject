import scala.math.log
import scala.collection.parallel.CollectionConverters._

object PageSearch {
    /**
     * @param pages  a list of RankedWebPage objects to be searched
     * @param query  a list of search terms to be counted in those pages
     * @return       a list of the number of times any of the terms appeared in each page in the same order as given
     */
    def count(pages: List[RankedWebPage], query: List[String]): List[Double] = {
  
        for(page <- pages) yield subCounter(page.text, 0, query)

    }
    def subCounter(pageText: String, sumSoFar: Double, query: List[String]): Double = {
        if pageText.isEmpty then return sumSoFar
        for(text<-query) if (text.length <= pageText.length) then if (pageText.substring(0, text.length).equals(text)) then return subCounter(pageText.tail, sumSoFar+1.0, query)
        subCounter(pageText.tail, sumSoFar, query)
    }
    def subCounter2(pageText: String, sumSoFar: Double, query: List[String], pages: List[RankedWebPage]): Double = {
        if pageText.isEmpty then return sumSoFar
        for(text<-query) if (text.length <= pageText.length) then if (pageText.substring(0, text.length).equals(text)) then return subCounter(pageText.tail, sumSoFar+1.0*(log(pages.length/(totalCounter(text, pages)+1))), query)
        subCounter(pageText.tail, sumSoFar, query)
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the term-frequency of the occurrences of those terms in each page in the same order given
     */
    def totalCounter(term: String, pages: List[RankedWebPage]): Double = {
        
        val countList = for(page <- pages) yield termCounter(term, page.text, 0)
        
        countList.sum
    }
    def termCounter(term: String, pageText: String, sumSoFar: Double): Double = {
        if pageText.isEmpty then return 0.0
        if(pageText.length >= term.length) then if  (pageText.substring(0, term.length).equals(term)) then return 1.0 
        termCounter(term, pageText.tail, sumSoFar)
    }
    def tf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        for(page <- pages) yield subCounter(page.text, 0, query) / page.text.length

    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the TF-IDF score for each page in the same order given
     */
    def tfidf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        for (page <- pages) yield subCounter2(page.text, 0, query, pages) / page.text.length
    }
}