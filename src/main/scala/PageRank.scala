import scala.annotation.tailrec
import scala.util.Random
import scala.collection.parallel.CollectionConverters.*

object PageRank {
    // empty WebPage for getOrElse shenanigans
    val emptyPage = WebPage("", "", "", "", List())

    /**
     * @param pages A map of page.id to page for some number of WebPage objects
     * @return      A map of page.id to a weight of 1.0 for those same WebPage objects
     */
    def equal(pages: Map[String, WebPage]): Map[String, Double] = {
        (for i <- pages.keys yield (i, 1.0)).toMap[String, Double]
    }

    /**
     * @param pages A map of page.id to page for some number of WebPage objects
     * @return A map of page.id to a weight that is a simple count of the number of pages linking to that page
     */
    def indegree(pages: Map[String, WebPage]): Map[String, Double] = {
        for page <- pages yield (page._1, (for link <- page._2.links if page._1 == link yield 1).sum.toDouble)
    }

    def pagerank(pages: Map[String, WebPage]): Map[String, Double] = {
        val walks: List[String] = (for i <- 1 to 100000 yield walk(100, pages.keys.toList(Random.nextInt(pages.keys.size)), pages)).toList // List of all 10000 walking results
        val walksMap = walks.foldLeft(Map.empty[String, Int])((num, link) => num + (link -> (num.getOrElse(link, 0) + 1)))
        walksMap.map((word, wordInt) => (word, wordInt.toDouble))
    }

    @tailrec
    def walk(walksLeft: Int, startingPage: String, pages: Map[String, WebPage]): String = {
        if (walksLeft <= 0) {
            step(pages.getOrElse(startingPage, emptyPage).links, pages)
        } else {
            val nextStep = step(pages.getOrElse(startingPage, emptyPage).links, pages)
            walk(walksLeft-1, nextStep, pages)
        }
    }

    /**
     * @param links: a list of Strings that the page this step is called for links to
     * @param pages A map of page.id to page for some number of WebPage objects
     * @return the next page in the walk
     */
    def step(links: List[String], pages: Map[String, WebPage]): String = {
        if links.nonEmpty && Random.nextDouble() >= .15 then links(Random.nextInt(links.size)) else pages.keys.toList(Random.nextInt(pages.keys.size))
    }
}