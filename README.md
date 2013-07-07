ngramr
======

R package to query the Google Ngram Viewer

The [Google Books Ngram Viewer](http://books.google.com/ngrams) allows you to enter a list
of phrases and then displays a graph showing how often the phrases have occurred in a
corpus of books (e.g., "British English", "English Fiction", "French") over time.
The underlying data is hidden in web page, embedded in some Javascript.
This package extracts the data an provides it in the form of an R dataframe.

The code was adapted from a Python script available from 
[Culturomics](http://www.culturomics.org/Resources/get-ngrams).
I believe it was written by [Jean-Baptiste Michel](https://twitter.com/jb_michel).

If you have `devtools` installed and loaded, you can install this package using the command
    
    install_github("ngramr", "seancarmody")
