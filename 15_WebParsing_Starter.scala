import scala.collection.parallel.CollectionConverters._ 

  

def getSource(url: String): Option[String] = { 

  try{ 

    Some(scala.io.Source.fromURL(url).mkString) 

  } catch { 

    case e: java.net.MalformedURLException => {println("Malformed URL: " + url); None} 

    case e: java.net.UnknownHostException => {println("Unknown host: " + url); None} 

    case e: java.io.FileNotFoundException => {println("Could not find file: " + url); None} 

    case e: Exception => {println(e.getClass() + ": " + e.getMessage() + ": " + url); None} 

  } 

} 

  

  

// from http://www.alexa.com/topsites/countries/US 

val top50UrlsUsa = Vector( 

"google.com", 

"youtube.com",  

"facebook.com", 

"amazon.com", 

"yahoo.com", 

"wikipedia.org", 

"reddit.com", 

"twitter.com", 

"ebay.com", 

"linkedin.com", 

"netflix.com", 

"diply.com", 

"instagram.com", 

"live.com", 

"craigslist.org", 

"bing.com", 

"imgur.com", 

"ntd.tv", 

"cnn.com", 

"pinterest.com", 

"tumblr.com", 

"office.com", 

"microsoftonline.com", 

"t.co", 

"chase.com", 

"nytimes.com", 

"blogspot.com", 

"imdb.com", 

"paypal.com", 

"wordpress.com", 

"espn.com", 

"apple.com", 

"breitbart.com", 

"msn.com", 

"walmart.com", 

"wikia.com", 

"bankofamerica.com", 

"salesforce.com", 

"wellsfargo.com", 

"washingtonpost.com", 

"weather.com", 

"intuit.com", 

"huffingtonpost.com", 

"zillow.com", 

"microsoft.com", 

"instructure.com", 

"foxnews.com", 

"twitch.tv").map( "http://" + _ ) 

  

def wordsOnly(s: String): Iterable[String] = { 

    val cleaned = s.map( (c: Char) => if (Character.isAlphabetic(c) || Character.isWhitespace(c) ) c else ' ' ) 

    cleaned.split("\\s").filter( _.length > 0) 

} 

  

def getDistinctWords(urls: Iterable[String], useParallel: Boolean = false): Iterable[String] = { 

  

    val srcStrings = if(useParallel){ 

       urls.par.flatMap( (url: String) => getSource(url) ).seq 

    } else { 

       urls.flatMap( (url: String) => getSource(url) ) 

    } 

     

    srcStrings.flatMap((src: String) => wordsOnly(src)).toSeq.distinct 

} 

  

  

def hasMostDistinctWords(urls: Iterable[String]): String = { 

    val srcStrings = urls.map( (url: String) => getSource(url) ) 

     

    val urlsWithSource = urls.zip(srcStrings).filter( 

       (pr: (String, Option[String])) => !pr._2.isEmpty ).map( 

       (pr: (String, Option[String])) => (pr._1, wordsOnly(pr._2.get)) ) 

     

    val urlsWithWordCounts = urlsWithSource.map( (pr: (String, Iterable[String])) => 

         (pr._1, pr._2.toSeq.distinct.length) ) 

          

    val res = urlsWithWordCounts.foldLeft( ("", -1) ) { 

        (best: (String, Int), next: (String, Int)) => 

            if(best._2 > next._2) best else next 

    } 

     

    res._1 

} 

  

def timeIt[A]( f: => A ) = { 

    val start = System.nanoTime 

    f 

    val end = System.nanoTime 

    (end - start).toDouble / 1000000.0 

} 