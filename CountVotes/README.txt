/*
    Copyright 2015 Silicon Econometrics Pty. Ltd. 

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.


 */


This is a program to count the senate votes in the NSW (Australia) state election.
It is designed for research purposes only, being deficient in a
variety of important ways compared to the specification at 
http://www.elections.nsw.gov.au/__data/assets/pdf_file/0009/171684/Functional_Requirements_for_Vote_Count_v3.2.pdf
  (1) It does not correctly implement tie resolution for rounding of partially transferred votes.
      (this can rarely affect the choice of one vote during a partial redistribution)
      This is step 18/19 from the specification.
  (2) Not seriously tested. It produces similar results to the official tally 
      (taking randomness into account), but has not been serious tested.
  (3) Not producing the required documentation (e.g. recording each PRN)
  (4) Is not reproducible with a given random seed, as, when a random selection is
      made of a set, the elements are not first sorted into a canonical order.
  (5) It often does things that are mathematically equivalent to the specification,
      but different for performance reasons. For instance, if choosing 900,000 out of 
      1,000,000 votes randomly, it will randomly pick the 100,000 to leave out rather than the
      larger number to leave in.
  (6) Random draws are done automatically; you can't physically draw them out of a hat.
  (7) Things I have forgotten or neglected.
Summary: This is provided in the hope it is useful but no guarantees of anything.

It can run multiple times in a multithreaded manner to investigate the effect of randomness,
taking 5s per run.


Compiling
---------
It is written in Scala. It was written in version 2.11, but should work with 
2.10 and probably future versions. It needs the standard scala libraries, plus
the xml library that comes with the scala distribution but is not generally
added by default to the classpath.

The directory structure includes files that allows it to be used as an eclipse
project using the scala plugin. If you are not using eclipse, just use the src project.
If using eclipse, you will need to set the path for the scala-xml library manually.


Running
-------

1) Get the state wide preferences data file from
   http://www.vtr.elections.nsw.gov.au/lc-home.htm#lc/state/preferences

2) Unzip it.

3) Change the following line in the file DataStructures.scala to the directory containing the unzipped file:
     val dir = "/Users/Andrew/Desktop/"
   
4) When the program runs it will parse this file, and create a simplified version ignoring
   various not needed things. Subsequent runs will use this file and be faster.
   It will also print out statistics on unique votes.
   (it will create a copy that is restricted to iVote results, and can be used
   to look at various statistics, such as in PreferencesStats)
   
5) Run "RunElection" as a scala application to run a single time. It will make a statistics
   file in the direction specified in the file ElectionReport :
     val dir = "/Users/Andrew/Desktop/Election Report/"
 
    *** VITAL: NOTE THAT THE PROGRAM WILL DELETE THE CONTENTS OF THIS DIRECTORY FIRST.
       DO NOT JUST MAKE IT YOUR DESKTOP OR OTHER IMPORTANT DIRECTORY.  ****
     
   The report will look prettier if you copy the file report.css into the parent directory
   of said directory. Start at index.html.
   
   Much information will also be printed to the console.

5) Run "RunProbabilistically" as a scala application to run many times. Set the lines
    val numRunsPerThread = 2500
    val numThreads = 4
   To the numbers required.
   After each of the first ten, and then every hundred, it will print out a summary
   of the results, including the margin for the final elimination, and for each
   candidate ever elected, the proportion of times elected and mean position.
   



General comments on code
------------------------

The code was written quickly and with an eye on performance. Don't expect a shining example
of software engineering.

DataStructures.scala reads the input formats and merges adjacent tallys.
ElectionReport.scala contains the result of the election and code to print out pretty reports
RunElection.scala contains the actual meat of the election.

For performance reasons, identical votes are gathered together and processed as a group.
This makes the random subset generation slightly more complex, but reduces the number
of votes to be dealt with by a factor of roughly 40, providing a significant speedup and
memory reduction.

Also for performance reasons, votes being counted consist of an array of preferences,
plus an integer offset into the array indicating candidates to be skipped due
to elimination or redistribution.

This means the basic data structure for a vote during the count is the DVote 
(distributable vote) in RunElection.scala, containing an offset and multiplicity
as well as the expected preference list.





   

