# ngramr 1.7.7

* Update for changes in ngram viewer website
* New corpus names (e.g. eng_2019 changed to en_2019)

# ngramr 1.7.6

* Drop use of lifecycle badges
* Add markdown format NEWS file

# ngramr 1.7.5

* Tidied fromJSON call
* Started to use lifecycle in documentation (ngrami)

# ngramr 1.7.4

* Imposed version dependency for dplyr to ensure relocate available

# ngramr 1.7.3

* Updated documentation to provide details of return values

# ngramr 1.7.2

* Change download code to use 'url' to ensure code works behind a proxy server
* Addressed CRAN submission requirements

# ngramr 1.7.1

* Change year_start default to 1800 in documentation

# ngramr 1.7.0

* Comprehensive refactor of underlying code
* More robust error/warning handling
* Dropped the "tag" argument from ngram functions

# ngramr 1.6.5

* Fix case_sensitive attribute

# ngramr 1.6.4

* Fix error in corpus count dataset

# ngramr 1.6.0

* Update to address issue (#26) resulting from change in the format of Google Ngram Viewer webpage

# ngramr 1.5.0

* Incorporated pull [changes #22, @seancarmody](https://github.com/seancarmody/ngramr/pull/22)
* Make wildcard searches expand to all terms
* Error out on server answer "Please try again later."    1a655f3
* Fix setting default corpus. 0b22dc4
* scale functions: do not explicitly set name, allow overwrite.   3a21061
* Allow passing through additional parameters to ngram_single.    ac6b1cc
* For wildcard searches, drop the cumulated (All) column
* Added travis-ci testing

# ngramr 1.4.5

* Fixed problems with (some) advanced operators


# ngramr 1.4.4

* Removed debugging from ngrami

# ngramr 1.4.3

* Fixed the Pulser bug

# ngramr 1.4.2

* Fix accented character encoding problem on Windows

# ngramr 1.4.1

* Improve ssl handling (refer Hadley's comment here: http://www.statsravingmad.com/blog/statistics/a-tiny-rcurl-headache/) 

# ngramr 1.4.0

Google has switched to SSL for the N-gram viewer and the format of the web-pages has
changed. This means that earlier versions of the package are completely broken. This
release fixes this major problem.

# ngramr 1.3.2

* Add README.md to .Rbuildignore to remove from CRAN

# ngramr 1.3.1

* Fix count for n-grams with n>1, including a "fudge" for 2012 corpuses

# ngramr 1.3.0

* Add option to display long-form corpus name
* Warn about smoothing >0 for geoms other than "line"
* Tidy documentation for print.ngram
* ngram and ngrami return S3 class "ngram"
* Format print for ngram objects
* ggram can take either a list of phrases or an ngram object

# ngramr 1.2.4

* Add option to relabel y-axis
* Add word counts option to ngram
* Change ggplot2 and scales from Requires to Suggests

# ngramr 1.2.3

* Prevent use of complex operators in case insensitive searchs
* Warn about character substitution

# ngramr 1.2.2

* CRAN release version
* More efficient handling of escaped Unicode (thanks Hadley http://stackoverflow.com/a/17787736/1543437)
* Fix package checking problems associated with plyr

# ngramr 1.2.1

* Tidy Google theme

# ngramr 1.2.0

* First semi-official release. All future development moved to the 'develop' branch.
* Allow case insensitive plotting with ggram
* Avoid reshape/reshape2 conflicts (thanks to Francois Briatte)
* Pass arbitrary geoms to `ggram`
* New function `ngramw` to return results in "wide" format
* Removed `wide` option from `ggram` and `ggrami`
* Better handling of legends when `ignore_case = TRUE`
* Error trapping long phrase lists
* Google theme option

# ngramr 1.1

* Added plot wrapper ggram
* Detect invalid corpus names

# ngramr 1.0

* Initial release of the ngramr package