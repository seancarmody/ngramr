ngramr
======

### R package to query the Google Ngram Viewer

The [Google Books Ngram Viewer](http://books.google.com/ngrams) allows you to enter a list of phrases and then displays a graph showing how often the phrases have occurred in a large corpus of books (e.g., "British English", "English Fiction", "French") over time. The current corpus collected in 2012 contains almost [half a trillion](http://languagelog.ldc.upenn.edu/nll/?p=4258) words for English alone.

The underlying data is hidden in Web page, embedded in some Javascript.
This package extracts the data and provides it in the form of an R dataframe. The code was adapted from a handy Python script available from 
[Culturomics](http://www.culturomics.org/Resources/get-ngrams).
It was written by [Jean-Baptiste Michel](https://twitter.com/jb_michel).

#### Installing

This package required R version 2.15 or higher. If you are using an older version of R
you will be prompted to upgrade when you try to install the package, so you may as well
upgrade now!

If you have [`devtools`](http://cran.r-project.org/web/packages/devtools/index.html)
installed, install this package directly from GitHub:

    require(devtools)
    install_github("ngramr", "seancarmody")
    require(ngramr)
   
and if you are feeling a little more adventurous, you can install the development version:

    install_github("ngramr", "seancarmody", "develop")

although it may not always work.

If you are behind a proxy, `install_github` may not work for you. Instead of fiddling around with the `RCurl` proxy settings, you can download the [ZIP archive](https://github.com/seancarmody/ngramr/archive/master.zip) and use `install_local` instead.

#### Examples

Here is an example of how to use the `ngram` function:

    require(ggplot2)
    ng  <- ngram(c("hacker", "programmer"), year_start = 1950)
    ggplot(ng, aes(x=Year, y=Frequency, colour=Phrase)) +
      geom_line()

The result is a ggplot2 line graph of the following form:

![Ngram Chart](http://i.imgur.com/EhSE9eK.png)

The same result can be achieved even more simply by using the `ggram` plotting wrapper that supports many options, as in this example:

![Ngram chart, with options](http://i.imgur.com/p5Q3pgM.png)

    require(ggplot2)
    ggram(c("monarchy", "democracy"), year_start = 1500, year_end = 2000, 
          corpus = "eng_gb_2012", ignore_case = TRUE, 
          geom = "area", geom_options = list(position = "stack")) + 
          labs(y = NULL)

The colors used by Google Ngram are available through the `google_theme` option, as in this example posted by Ben Zimmer [at Language Log](http://languagelog.ldc.upenn.edu/nll/?p=4979):

![Ngram chart, with Google theme](http://i.imgur.com/qKHvQA4.png)

    require(ggplot2)
    ng <- c("(The United States is + The United States has) / The United States",
          "(The United States are + The United States have) / The United States")
    ggram(ng, year_start = 1800, google_theme = TRUE) +
      theme(legend.direction = "vertical")

#### Further Reading

For more information, read [this Stubborn Mule post](http://www.stubbornmule.net/2013/07/ngramr/) and the [Google Ngram syntax](http://books.google.com/ngrams/info) documentation. If you would rather work with R and SQL on the raw Google Ngram datasets, [see this post](http://rpsychologist.com/how-to-work-with-google-ngram-data-sets-in-r-using-mysql/).
