####This module implement rest api logic.

In order to build you need to run 
1) 'sbt rest/openApiGenerate'
2) 'sbt rest/run'
3) In order to clean up generated sources there is clean.bat script prepared

Server will be available on address localhost:9000

Note: generated sources should not be committed to git repository.\
Note2: probably generating sources at compile time as well as cleaning generated sources could be done using sbt, but it hasn't been achieved due to lack of time