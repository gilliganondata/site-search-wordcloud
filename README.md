# Exploring Site Search with Google Analytics

This is a Shiny app that works with Google Analytics data to explore site search usage on a site. There are three main components of what it does -- two of which are ideas taken from others and then coded into the app here:

* **"Stemming" of site search terms** -- [Sébastien Brodeur](https://twitter.com/SebastienBrodeu) did a demo at Superweek 2017 of how he collapsed the variations of search terms into a single "stemmed" term. This makes for more meaninful frequency counts.
* **Selective removal of terms** -- many sites have some "dominant" search terms that are valid...but that dwarf the ability to get to the really interesting stuff. This app allows the user to simply type in words to remove them from the frequency counts and word cloud.
* **Questions in search** -- this was something [Nancy Koons presented a few years ago](https://nanalytics.wordpress.com/2014/07/14/who-what-where-when-why-how-harnessing-the-power-of-internal-site-search/) -- filter down to just the searches that start with a "question word." These are searches well out on the long tail of searches, but they can be very insightful

You can see the application in action (you have to have access to a Google Analytics account) at [https://gilligan.shinyapps.io/ga-site-search/](https://gilligan.shinyapps.io/ga-site-search/).
