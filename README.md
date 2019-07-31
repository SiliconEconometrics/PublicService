# Count Australian STV elections

This contains a variety of programs for counting STV elections in
Australia; in particular for
*  Federal Senate
*  NSW local and legislative council
*  Victorian Senate (little tested)
*  WA Senate (little tested)

More information about more complex usage is included in the CountVotes/README.txt file. This file
focuses on compiling and running for someone not used to Scala.

The program is written in Scala 2.12, and can be compiled with 
sbt (a make like tool for Scala). 
You will need some Java virtual machine on your computer, and sbt.
I assume a Java virtual machine is already installed. 

## To install sbt

If sbt is not installed, you can download it from
https://www.scala-sbt.org/download.html
This can be done via the package manager, or without
packages. If you download the .tgz file, then
uncompress it somewhere:

```bash
tar xfz ~/Downloads/sbt-1.2.8.tgz
```
This will make a directory sbt. The sbt progran can be
run via `sbt/bin/sbt`.

## To compile

First clone the repository.
```bash
git clone https://github.com/SiliconEconometrics/PublicService.git
```

Now run sbt. I am assuming at this point that it was
untar'd in the same subdirectory as we were in when cloning
the git archive. If not, no problem, just adjust the path to
sbt in the command below:
```bash
cd PublicService/CountVotes/
../../sbt/bin/sbt assembly
```

This should produce text starting off like
```text
[info] Updated file /opt/arc/demo/PublicService/CountVotes/project/build.properties: set sbt.version to 1.2.8
[info] Loading settings for project global-plugins from idea.sbt ...
[info] Loading global plugins from /home/arc/.sbt/1.0/plugins
...
```
and finishing something like
 
 ```text
[info] Packaging /opt/arc/demo/PublicService/CountVotes/target/scala-2.12/CountPreferentialVotes-assembly-1.0.jar ...
[info] Done packaging.
[success] Total time: 14 s, completed 31 Jul. 2019, 10:21:45 pm
```

The time taken will be longer if you have not used sbt before; 
It will download any needed libraries and their transitive 
dependencies, which can seem like half the internet.

Anyway, it has now compiled to a jar file (runnable with a java virtual machine) at the location listed
on the line started `[info] Packaging`. 

## To Run

The JAR file should be executable. Run with a command like the following, changing the path to the JAR file
to match the output of sbt.
```bash
java -jar /opt/arc/demo/PublicService/CountVotes/target/scala-2.12/CountPreferentialVotes-assembly-1.0.jar
```

It should print out a list of allowable arguments and terminate. The most important of these is
the `--stv` argument which is used to provide a file listing all the candidates, groups, and votes.
This can be downloaded for the 2013, 2016 and 2019 Federal elections from https://vote.andrewconway.org
You will probably want to specify also the `--NumSeats` command which specifies how may people
should be elected (generally 6, except 2 in ACT and NT, doubled in a double dissolution like 2016).

We will show an example for the ACT, 2019. The directories used are arbitrary, of course

Make a working directory:
```bash
cd ~
mkdir ACT
cd ACT
```

Download the .stv file
```bash
wget https://vote.andrewconway.org/federal/2019/ACT/AndrewFormat -O federal_2019_ACT.stv
```
This downloads a 1.89MB file federal_2019_ACT.stv

Now run the election

```bash
java -jar /opt/arc/demo/PublicService/CountVotes/target/scala-2.12/CountPreferentialVotes-assembly-1.0.jar --stv federal_2019_ACT.stv --NumSeats 2
```

This will produce output like
```text
2019 Federal : ACT
SATLs : 0
RATLS : 210204 num distinct = 7183
BTLs : 60027 num distinct = 50131
Total formal votes : 270231
Total informal votes : 0
Candidates : 17
Running count for ACT
Winner : Katy GALLAGHER
Winner : Zed SESELJA
```
This says that there were 0 SATLs (ticket above the line votes), 210204 above the line votes, and 60027 below the line
votes. There were no informal votes (or at least none published by the AEC that I found). There were 17 candidates, and
the two elected candidates were listed. A full distribution of preferences is in a newly created subdirectory
`Federal2019Reports/ACT/About.html`

