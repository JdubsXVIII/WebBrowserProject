import scala.math.log
import scala.collection.parallel.CollectionConverters._

object PageSearch {
    /**
     * @param pages  a list of RankedWebPage objects to be searched
     * @param query  a list of search terms to be counted in those pages
     * @return       a list of the number of times any of the terms appeared in each page in the same order as given
     */
    def count(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        List() // TODO: implement this method and remove this stub
        for(page <- pages) yield subCounter(page.text, 0, query)

    }
    def subCounter(pageText: String, sumSoFar: Double, query: List[String]): Double = {
        if pageText.isEmpty then return sumSoFar
        for(text<-query) if (pageText.substring(0, text.length).equals(text)) then return subCounter(pageText.tail, sumSoFar+1.0, query)
        subCounter(pageText.tail, sumSoFar, query)
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the term-frequency of the occurrences of those terms in each page in the same order given
     */
    def tf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        List() // TODO: implement this method and remove this stub
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the TF-IDF score for each page in the same order given
     */
    def tfidf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        List() // TODO: implement this method and remove this stub
    }
}