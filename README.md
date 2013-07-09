ngramr
======

### R package to query the Google Ngram Viewer

The [Google Books Ngram Viewer](http://books.google.com/ngrams) allows you to enter a list
of phrases and then displays a graph showing how often the phrases have occurred in a
corpus of books (e.g., "British English", "English Fiction", "French") over time.
The underlying data is hidden in web page, embedded in some Javascript.
This package extracts the data an provides it in the form of an R dataframe.

The code was adapted from a handy Python script available from 
[Culturomics](http://www.culturomics.org/Resources/get-ngrams).
It was written by [Jean-Baptiste Michel](https://twitter.com/jb_michel).

If you have [`devtools`](http://cran.r-project.org/web/packages/devtools/index.html)
installed and loaded, you can install this package
using the command
    
    install_github("ngramr", "seancarmody")

If you are behind a proxy, `install_github` may not work for you. Instead of fiddling around with the `RCurl` proxy settings, you can download the [zip archive](https://github.com/seancarmody/ngramr/archive/master.zip) and use `install_local` instead.

Here is an example of how it is used.

    library(ngramr)
    library(ggplot2)

    ng  <- ngram(c("hacker", "programmer"), year_start=1950)
    ggplot(ng, aes(x=Year, y=Frequency, colour=Phrase)) + geom_line()

The result is a chart like this:

![Ngram Chart](http://farm8.staticflickr.com/7320/9230489654_1a510bd9b0_z.jpg)
