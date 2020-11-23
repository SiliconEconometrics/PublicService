/*
    Copyright 2016-2019 Silicon Econometrics Pty. Ltd.

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

package org.greatcactus.vote.count.margin

import org.greatcactus.vote.count.ElectionResultReport
import org.greatcactus.vote.count.MainDataTypes.{CandidateIndex, Tally, TallyMaxValue, TallyUnscaled}
import org.greatcactus.vote.count.ballots.ElectionData
import org.greatcactus.vote.count.margin.TransferResolution.CannotFindTamperableVotesException
import org.greatcactus.vote.count.weighted.{ContinuingCandidates, TamperableVotes, WeightedCountHelper}

/**
  * One way of computing the margin is to see how many votes need to be
  * tampered with to change the outcome of an election. This is hard to
  * calculate for STV. So the purpose of this code is to compute an
  * upper bound on the margin by finding examples of particular tamperings
  * that change the result of the election. Heuristics are used to find
  * what is worth changing.
  *
  * There are four ways to change the outcome of an election:
  *   (1) Get an official winner excluded at some count.
  *       This will clearly change the outcome.
  *   (2) Get an official loser to get a quota at some count
  *       This will clearly change the outcome, but will generally take a lot of manipulation and is thus not tried.
  *   (3) Change the set of people present in the last round (winners on less than a quota, plus last official loser).
  *       This requires (1) or (2) before, or getting someone who won on less than a quota to get a quota (inefficient),
  *       or getting the last official loser eliminated on some earlier round (plausible, a simple generalization of (1).
  *   (4) Having the same people present in the last round, but having a different elimination order leading to
  *       a different number of votes lost to rounding. This is unlikely to have an effect, and so is not
  *       covered here.
  *   (5) Having the same people present in the last round, but moving votes so that a different person loses.
  *       This is a very similar process to (1)
  *
  * So, the approach is to get a set W consisting of the winning candidates, plus the last loser, and trying to get
  * one of them eliminated at each time step, including the virtual round after the last round.
  *
  * For people not absurdly familiar with Australian STV mechanics, the reason the last round is special is that
  * most Australian STV systems have a special case in the legislation for the last round - if there are n+1
  * continuing candidates at the end of a round, and n seats to be filled, then instead of having another
  * elimination round, the election is just called as if there were one. This doesn't change the outcome, it
  * just changes the logistics and introduces an annoying special case.
  *
  * The way to get someone eliminated is to get them to have the least votes of continuing candidates.
  * This can be done either by reducing their votes or adding votes to the candidates with tallies below
  * said candidate.
  *
  * It could also be (possibly partially) done, in a more complex manner, by causing someone
  * else to get eliminated (taking n manipulations) who would transfer more than n more votes to the people
  * below the candidate to be eliminated than to said candidate. [ NOT CURRENTLY DIRECTLY DONE ]
  *
  * Note that this is done from the perspective of trying to tamper with the existing results to make
  * a different result; but this is symmetric to the case of trying to find what the original may have
  * been before some hypothetical malicious actor tampered with the votes to produce the official results.
  * But it is easier to think about it forwards than reverse.
  *
  */

object Tampering {

  /** Normally, at each round one makes a concerted effect to unseat an elected candidate. If this is true, then try everyone. This rarely helps and significantly increases time. Recommended false. */
  val shouldTryToExcludeEveryoneNotJustInLastRound : Boolean = false
  /** Whether we are at all interested in changing ATL votes. Recommended true */
  val shouldConsiderUsingATL : Boolean = true
  /** Whether we should do the expensive binary search on tampers excluding 1 prefs. Recommended true */
  val shouldOptimizeBinarySearchRerunElection : Boolean = true
  /** Whether we should do the expensive binary search on tampers excluding 1 prefs. Recommended false but borderline */
  val shouldOptimizeBinarySearchRerunElectionEvenFor1Prefs : Boolean = false
  /** Whether we should try to deal with taking votes from people other than the new loser if they have a low transfer value. Recommended true */
  val optimizeLowTransferValuesFromNewLoser : Boolean = true
  /** Whether we should try taking votes from just people who are not the new loser. Recommended true */
  val tryIndirectExclusion : Boolean = true


  def lookForOportunitiesToTamperThisRound(
                                            helper:WeightedCountHelper,
                                            candidatesThatWouldNormallyBeExcludedThisRound:List[CandidateIndex],
                                            normalElectionOutcome: NormalElectionOutcome,
                                            //marginOptions:WhatMarginInformationToCompute,
                                            /** If this is being done after the step is complete (at end of election, special case ending). */afterStepCount:Boolean
                                          ) : Unit = {
    val tallys: Array[TallyUnscaled] = helper.tallys.map{_/helper.voteMultipleToDealWithFractionalVotes}
    val quota = helper.quota/helper.voteMultipleToDealWithFractionalVotes
    def continuingCandidates: ContinuingCandidates = helper.continuingCandidates
    def report: ElectionResultReport = helper.report
    val data: ElectionData = helper.data
    def getTamperableVotes(from:CandidateIndex,allowATL:Boolean,use1Pref:Boolean) : List[TamperableVotes] = helper.ballots(from).getTamperableVotes(data.candidates(from).position-1,wantActualVotes = true,allowATL,use1Pref,helper.voteMultipleToDealWithFractionalVotes)
    def name(index:CandidateIndex): String = data.candidates(index).name
    def countNumber: Int = report.history.length+(if (afterStepCount) 0 else 1)

    val continuingLowToHigh: Array[CandidateIndex] = continuingCandidates.orderedList.reverse.toArray
    /** The likely tally to get below to get excluded at this count. */
    //val excludeCutoff: Tally = candidatesThatWouldNormallyBeExcludedThisRound.map{tallys(_)}.max

    /** Candidates at the top of their party, or who have all elected above them, and this can receive or give away ATL votes. */
    val canUseATL : Set[CandidateIndex] = {
      val candidates = data.candidates
      def topOfRemaining(inGroup:List[CandidateIndex]) : CandidateIndex = inGroup.minBy(candidates(_).position)
      continuingCandidates.orderedList.groupBy { candidates(_).group }.filter{_._1 != "UG"}.map{case (_,candidatesInGroup)=>topOfRemaining(candidatesInGroup)}.toSet
    }

    /** Get votes that could conceivably be taken away from a given candidate */
    def getGiveAwayableVotes(from:CandidateIndex,useATL:Boolean,use1Pref:Boolean): List[CanGiveAwayVotes] = {
      val tamperableList: List[TamperableVotes] = getTamperableVotes(from,allowATL = useATL,use1Pref)
      for (tamperable<-tamperableList if tamperable.votes>0) yield CanGiveAwayVotes(from,tamperable)
    }

    /** If a result works, save it. */
    def processResult(result:Option[MarginAndResults],useATL:Boolean): Unit = {
      for (mar<-result) {
        println(mar.margin.how(data.candidates))
        println("Winners = "+mar.results.winners.mkString(","))
        if (mar.margin.hasATL && !useATL) throw new IllegalArgumentException("Should not have used ATL votes!")
        if (normalElectionOutcome.winners.toSet==mar.results.winners.toSet) throw new IllegalArgumentException("Did not accomplish anything")
        report.marginsRecorder.add(mar.margin, ElectionChanged(normalElectionOutcome.winners,mar.results.winners))
      }
    }


    /** Optimize votesNeededByLowerThanCToPassC by binary searching on lower values and rerunning the election to see if it works.
      * This is done individually for each candidate, as often a particular candidate will be significantly lower than expected, if it is OK for them to be eliminated early.
      * Returns optimized list, and the actual election run details.
      *
      * if !actuallyDoSearch, then just sanity check that the provided results work.
      * */
    def binarySearchOptimizeVotesNeeded(surplusVotesAvailable:List[CanGiveAwayVotes],votesNeededByLowerThanCToPassC : List[(CandidateIndex,Tally)],actuallyDoSearch:Boolean,preferredCandidateToTransferAwayFrom:Set[CandidateIndex],debug:Boolean=false) : Option[(MarginAndResults,List[(CandidateIndex,Tally)])] = {
      /** change the list of needed votes by changing a particular candidate's index to something else. */
      def change(baseVotesNeeded:List[(CandidateIndex,Tally)],changingCandidate:CandidateIndex,changeTo:Tally) : List[(CandidateIndex,Tally)] = {
        if (changeTo == 0) baseVotesNeeded.filter(_._1 != changingCandidate)
        else baseVotesNeeded.map { case (who, howmany) => if (who == changingCandidate) (who, changeTo) else (who, howmany) }
      }
      /** Get the specific papers involved in providing needed votes from the start of the surplusVotesAvailable list. Throws CannotFindTamperableVotesException if not possible. */
      def getTransfers(surplusVotesAvailable:List[CanGiveAwayVotes],neededVotes:List[(CandidateIndex,Tally)]) : TransferResolution =  TransferResolution.getTransfers(surplusVotesAvailable,neededVotes,canUseATL,preferredCandidateToTransferAwayFrom)
      /** Return Some(results) if candidate c does not get elected with the given list of votes removed. */
      def rerunToSeeIfCandidateChanged(votesNeeded:List[(CandidateIndex,Tally)]) : Option[MarginAndResults] = {
        try {
          val transfers = getTransfers(surplusVotesAvailable,votesNeeded)
          val margin = transfers.toMargin(countNumber)
          //println("Transfers = "+transfers)
          val result = helper.rerunElectionWithModifiedData(Some(margin), name="bso"+countNumber+" candidate "+preferredCandidateToTransferAwayFrom.mkString(","),None)
          //if (changingCandidate!= -1) println("Trying to change candidate "+name(changingCandidate)+" to "+changeTo+" to try to exclude "+name(c)+" winners = "+newWinners)
          if (result.winners.toSet==normalElectionOutcome.winners.toSet) None else Some(new MarginAndResults(margin,result))
        } catch { case _:CannotFindTamperableVotesException => None }
      }
      var votesNeeded = votesNeededByLowerThanCToPassC
      if (actuallyDoSearch) { // for each candidate, work out the minimum number of votes needed by binary search on a rerun election.
        import scala.util.control.Breaks._
        breakable { for (_<- 1 to 2) { // may improve with 2 passes
          //println("Starting pass "+votesNeeded)
          val stateAtStartOfPass = votesNeeded
          for ((changingCandidate,currentNeeded)<-votesNeeded) {
            def works(number:Tally) : Boolean = rerunToSeeIfCandidateChanged(change(votesNeeded,changingCandidate,number)).isDefined
            val actuallyNeeded = if (votesNeeded.length<5) { // do full binary search
              //println("Starting bsearch needed="+currentNeeded)
              BinarySearch.findLowest(works : Tally=>Boolean,0,currentNeeded)
            } else { // just try setting to zero, a common solution
              if (works(0)) 0:Tally else currentNeeded
            }
            votesNeeded=change(votesNeeded,changingCandidate,actuallyNeeded)
          }
          if (votesNeeded == stateAtStartOfPass) break // don't waste time with another pass
        }}
      }
      for (res<-rerunToSeeIfCandidateChanged(votesNeeded)) yield (res,votesNeeded) // sanity check that the result works at all.
    }


    /** The workhorse. Try to exclude the candidate, possibly using ATL votes. */
    def tryToExcludeCandidate(c:CandidateIndex,useATL:Boolean): Unit = {
      //println("processCandidate("+c+" "+data.candidates(c).name+","+useATL+")")
      val availableFromCNo1Prefs: List[CanGiveAwayVotes] = getGiveAwayableVotes(c,useATL,use1Pref = false)
      lazy val availableFromCWith1Prefs: List[CanGiveAwayVotes] = getGiveAwayableVotes(c,useATL,use1Pref = true)

      if (availableFromCNo1Prefs.exists(_.votes.requiresAlteringFirstPreference)) throw new IllegalArgumentException


      /** work out who gets the votes for the simple case of candidate c giving some specific number of their votes to
        * the candidates below c. "Fills up" in order to maximise the minimum number of votes that someone has.
        * eg. If the tallys are  L:100 M:10 N:5 O:3 then the first two will go to O, and the next 2*5 will be split amongst N and O, then M will start getting them too.
        */
      def computeRecipients(votesLost:Tally) : Array[TamperedVoteNoPapers] = { // work out who gets the votes
        val votesGivenTo : Array[Tally] = new Array[Tally](continuingLowToHigh.length) // votes given to candidate by index position
        var numGivenTo = 0 // number of candidates given votes
        var togo = votesLost
        def tally(index:Int) = { // modified tally due to getting extra votes
          if (index>=continuingLowToHigh.length) {
            if (!afterStepCount) throw new IllegalArgumentException("This should only happen if c is not a continuing candidate, which only makes sense to consider at the last step of the election.")
            TallyMaxValue
          } else {
            val who = continuingLowToHigh(index)
            if (who==c) TallyMaxValue else votesGivenTo(index)+tallys(who)
          }
        }
        def give(index:Int,n:Tally) { votesGivenTo(index)+=n; togo-=n }
        while (togo>0) {
          numGivenTo+=1
          val toCatchUp = tally(numGivenTo)-tally(numGivenTo-1) // distribute all votes until the next smallest candidate is reached.
          val maxToAll = togo/numGivenTo
          val giveEach = maxToAll min toCatchUp
          for (i<-0 until numGivenTo) give(i,giveEach)
          if (giveEach<toCatchUp) // distribute remaining lot, weakest candidate first
            for (i<-0 until togo.toInt) give(i,1) // togo < numGivenTo < numCandidates
        }
        val result = for (i<-0 until numGivenTo if votesGivenTo(i)>0) yield new TamperedVoteNoPapers(c,continuingLowToHigh(i),votesGivenTo(i))
        result.toArray
      }
      /** A simple margin assuming you are taking a that number of votes from c and giving it to other people. The
        * minimum needed to make sure that c is excluded at the current step. Doesn't actually run the election, uses the helper. */
      val simpleMarginJustTakingFromC : Tally = {
        def isExcludedByTransferringYourVotesToOthers(votesLost:Tally) : Boolean = { // see if candidate c is excluded if s/he loses votesLost votes.
          //println(s"trying to exclude $votesLost for candidate $c tally "+tallys(c))
          val recipients = computeRecipients(votesLost)
          val localTallys : Array[Tally] = tallys.clone()
          localTallys(c)-=votesLost
          for (recipient<-recipients) localTallys(recipient.candidateTo)+=recipient.numVotes
          val newOrdered = continuingCandidates.orderedList.sortBy { -localTallys(_) }
          val nowExcluded = helper.hypotheticalCandidatesForExclusion(newOrdered,localTallys)
          //println("Ordered : "+newOrdered+" nowExcluded="+nowExcluded.mkString(",")); Thread.sleep(300)
          nowExcluded.contains(c)
        }
        BinarySearch.findLowest(isExcludedByTransferringYourVotesToOthers : Tally=>Boolean,0:Tally,tallys(c)-tallys(continuingLowToHigh(0))+1)
      }

      def tryJustTakingVotesFromTheCandidate(votesLost:Tally,requireTamperable:Boolean) : Option[MarginAndResults] = {
        val surplusVotesAvailable:List[CanGiveAwayVotes] = if (requireTamperable) availableFromCNo1Prefs else availableFromCWith1Prefs
        val numSpare : TallyUnscaled = surplusVotesAvailable.map{_.votes.votes}.sum-votesLost
        if (numSpare>=0) {
          val doExpensiveSearch = if (requireTamperable) shouldOptimizeBinarySearchRerunElection else shouldOptimizeBinarySearchRerunElectionEvenFor1Prefs
          val fudgeFactorForRoundingWhenDeterminingTamperMargins : TallyUnscaled = if (doExpensiveSearch) (20:TallyUnscaled) min numSpare else 0:TallyUnscaled // edit this by hand if needed. The minimum number is to prevent it failing gratuitously because of the added 20. The added 20 is to look after funny misses due to rounding.
          //val totalAvailable =
          val neededVotes:List[(CandidateIndex,Tally)] = computeRecipients(votesLost+fudgeFactorForRoundingWhenDeterminingTamperMargins).map{tvnp=>(tvnp.candidateTo,tvnp.numVotes)}.toList
          binarySearchOptimizeVotesNeeded(surplusVotesAvailable,neededVotes,doExpensiveSearch,Set(c)).map{_._1}
        } else None // it is possible that there is something missed here, as leaving something out could make it suddenly possible.
      }

      /**
        * Try taking votes from people other than the candidate, and giving them to people scoring below the candidate to make the candidate be excluded.
        * @param allowFromCAsWell if true, then can also take from C.
        * @return the best margin one can find, if any.
        */
      def tryTakingVotesFromPeopleOtherThanTheCandidate(allowFromCAsWell:Boolean) : Option[MarginAndResults] = {
        val requireTamperable = true
        val doExpensiveSearch = if (requireTamperable) shouldOptimizeBinarySearchRerunElection else shouldOptimizeBinarySearchRerunElectionEvenFor1Prefs
        val fudgeFactorForRoundingWhenDeterminingTamperMargins = if (doExpensiveSearch) 20 else 0 // edit this by hand if needed.  This gives some wiggle room if the apparently-perfect manipulation actually doesn't quite change the answer.
        // possibly this should be reduced, as it is a per-candidate number here.
        // At the moment, it's set to zero when binary-search is turned off - a larger value would be much less likely to fail, but may not be near optimal.
        //println("searchWithIndirectionHardToDetect("+allowFromC+")")
        val reallyAvailableFromCtamperable : List[CanGiveAwayVotes] = if (allowFromCAsWell) availableFromCNo1Prefs else Nil
        val assumeTakenFromC = reallyAvailableFromCtamperable.map{_.votes.votes}.sum min simpleMarginJustTakingFromC
        val tallyToMoveEveryoneElseTo : Tally = tallys(c)-assumeTakenFromC+fudgeFactorForRoundingWhenDeterminingTamperMargins+1 // as long as candidates don't drop below here, they won't go in front of candidate. Unless maybe binary search makes someone else get eliminated first... but it is still a good heuristic.
        //println("assumeTakenFromC="+assumeTakenFromC+"  simpleMarginJustTakingFromC="+simpleMarginJustTakingFromC+"  tallys("+c+")="+tallys(c)+" tallyToMoveEveryoneElseTo="+tallyToMoveEveryoneElseTo)
        /** The number of votes that could be taken away from various candidates, while still keeping their tally >=tallyToMoveEveryoneElseTo. Includes some negative values. May include positive values from people with a lower tally than the candidate. */
        val surplusVotes : List[(CandidateIndex,Tally)] = continuingCandidates.orderedList.map{nc=>(nc,tallys(nc)-tallyToMoveEveryoneElseTo)}
        /** The specific votes that people can give away. No attempt is made to choose which ones are the best. */
        val availableFromOthers : List[CanGiveAwayVotes] = surplusVotes.flatMap{
          case (who,surplus) if surplus>fudgeFactorForRoundingWhenDeterminingTamperMargins && who!=c =>
            val untruncated = getTamperableVotes(who,allowATL = useATL,use1Pref = false)
            val buffer = new collection.mutable.ListBuffer[TamperableVotes]
            var remainingSurplus = surplus-fudgeFactorForRoundingWhenDeterminingTamperMargins
            for (t<-untruncated) if (remainingSurplus>0) {
              val nt = if (remainingSurplus>=t.votes) t else t.scaleVotesBackTo(remainingSurplus)
              remainingSurplus-=nt.votes
              if (nt.votes>0) buffer+=nt
            }
            for (tamperable<-buffer.toList) yield CanGiveAwayVotes(who,tamperable)
          case _ => Nil
        }
        val availableFromOthersSorted = availableFromOthers.sortBy(- _.votes.tv)
        val allVotesAvailable = reallyAvailableFromCtamperable++availableFromOthersSorted
        //println("SWI round="+countNumber+" c="+c+" "+data.candidates(c).name+" surplusVotes="+surplusVotes)
        /** The votes needed to get people below C up to tallyToMoveEveryoneElseTo */
        val votesNeededByLowerThanCToPassC1 : List[(CandidateIndex,Tally)] = surplusVotes.dropWhile(_._1 !=c).tail.map{pair=>(pair._1,(0:Tally) max -pair._2)}
        /** Re-order said list such that people who can't received ATL votes are dealt with first, so the BTL votes aren't unnecessarily wasted on them. */
        val votesNeededByLowerThanCToPassC2 : List[(CandidateIndex,Tally)] = ListUtil.moveToFront(votesNeededByLowerThanCToPassC1)({pair=> !canUseATL(pair._1)})
        //val extraNeededToCompensateForFewerTakenFromCThanExpected = 0 max Math.ceil((assumeTakenFromC-votesNeededByLowerThanCToPassC1.map{_._2}.sum+fudgeFactorForRoundingWhenDeterminingTamperMargins+1).toDouble/(1+votesNeededByLowerThanCToPassC1.length)).toInt
        //println("extraNeededToCompensateForFewerTakenFromCThanExpected="+extraNeededToCompensateForFewerTakenFromCThanExpected)
        //val extraJustAvailable : Tally = ((allVotesAvailable.map { _.votes.votes }.sum-votesNeededByLowerThanCToPassC2.map{_._2}.sum)*0.5/(1 max votesNeededByLowerThanCToPassC2.length)).toInt max 0
        // Could add in an extra fudge factor here if need by, just as long as it is no bigger than extraJustAvailable. But it won't really solve the problems, as the binary search will reduce someone down, and then future reductions in C will cause problems back to that person.
        val extraUsed = 0 // extraNeededToCompensateForFewerTakenFromCThanExpected max extraJustAvailable
        // could possibly increase as much as you can - binary search on getTransfers - could produce better results
        val votesNeededByLowerThanCToPassC : List[(CandidateIndex,Tally)]  = votesNeededByLowerThanCToPassC2.map{pair=>(pair._1,extraUsed+pair._2)}
        //println("SWI round="+countNumber+" c="+c+" "+data.candidates(c).name+" allVotesAvailable="+allVotesAvailable+" votesNeededByLowerThanCToPassC="+votesNeededByLowerThanCToPassC)
        for ((mar,votesNeededOptimized)<-binarySearchOptimizeVotesNeeded(allVotesAvailable,votesNeededByLowerThanCToPassC,doExpensiveSearch,Set(c))) yield {
          if (mar.margin.papers<=0) {
            println("Had optimizedMargin.papers="+mar.margin.papers+" with votesNeededOptimized="+votesNeededOptimized+" votesNeededByLowerThanCToPassC="+votesNeededByLowerThanCToPassC+" allVotesAvailable="+allVotesAvailable)
          }
          if (optimizeLowTransferValuesFromNewLoser && allowFromCAsWell) { // try, one at a time (while things are improving) taking the lowest transfer value from the potential transfers from C.
            var bestMargin : MarginAndResults = mar
            var fromC = reallyAvailableFromCtamperable
            var extraVotesNeeded : Tally = 0
            //println("Trying optimizeLowTransferValuesFromNewLoser fromC="+fromC+" extraVotesNeeded="+extraVotesNeeded)
            while (fromC.nonEmpty) {
              extraVotesNeeded+=fromC.last.votes.votes
              fromC = fromC.take(fromC.length-1)
              val allVotesAvailable1 = fromC++availableFromOthersSorted
              //println("Trying optimizeLowTransferValuesFromNewLoser fromC="+fromC+" extraVotesNeeded="+extraVotesNeeded+" allVotesAvailable1="+allVotesAvailable1)
              binarySearchOptimizeVotesNeeded(allVotesAvailable1,votesNeededOptimized.map{case (cand,tally)=>(cand,tally+extraVotesNeeded+40)},doExpensiveSearch,Set(c)) match { // consider reducing 40 // candidates may need extra votes to pass
                case Some((mar1,_)) if mar1.margin.papers<bestMargin.margin.papers => bestMargin=mar1 // it got better!
                case _ => fromC=Nil // break out of loop
              }
            }
            bestMargin
          } else mar
        }
      }

      // Try various options

      println("Trying to exclude candidate "+c+" useATL="+useATL+" continuing candidates = "+continuingLowToHigh.mkString(",")+" with tallys "+continuingLowToHigh.map(tallys).mkString(",")+" simpleMarginJustTakingFromC="+simpleMarginJustTakingFromC+" afterStepCount="+afterStepCount)
      //println("try 1")
      // there is not point allowing to move first preferences above the line... you can do anything! Maybe we could restrict the number?
      if (!useATL) processResult(tryJustTakingVotesFromTheCandidate(simpleMarginJustTakingFromC,requireTamperable = false),useATL) // try tampering that would be detectable by good scrutineers

      //println("try 2")
      val simpleTry = tryJustTakingVotesFromTheCandidate(simpleMarginJustTakingFromC,requireTamperable = true) // try tampering that would not be detectable by good scrutineers not given access to all computer systems.
      processResult(simpleTry,useATL)

      //println("try 3")
      if (optimizeLowTransferValuesFromNewLoser || simpleTry.isEmpty) { // try a less efficient method - give away all the votes you can, and then take the remainder from elsewhere.
        processResult(tryTakingVotesFromPeopleOtherThanTheCandidate(allowFromCAsWell = true),useATL)
      }
      //println("try 4")
      if (tryIndirectExclusion) {
        // try tampering that would be hard to detect and is indirect - doesn't take any preferences from new loser, but may give some to the new winner. Hopefully not.
        processResult(tryTakingVotesFromPeopleOtherThanTheCandidate(allowFromCAsWell = false),useATL)
      }
    }

    /** The backup workhorse. Try to give the candidate a quota, possibly using ATL votes. */
    def tryToGiveQuotaToCandidate(c:CandidateIndex,useATL:Boolean,allow1Prefs:Boolean): Unit = {
      val availableFromOthers : List[CanGiveAwayVotes] = (for (who<-continuingCandidates.orderedList if who!=c) yield {
        val tamperable: Seq[TamperableVotes] = getTamperableVotes(who,allowATL = useATL,allow1Prefs)
        for (t<-tamperable) yield CanGiveAwayVotes(who,t)
      }).flatten.sortBy{- _.votes.tv}
      val doExpensiveSearch = if (!allow1Prefs) shouldOptimizeBinarySearchRerunElection else shouldOptimizeBinarySearchRerunElectionEvenFor1Prefs
      val votesNeeded = quota - tallys(c)
      val votesAvailable = availableFromOthers.map{_.votes.votes}.sum
      val availableForFudge = (votesAvailable-votesNeeded) max 0
      val votesNeededWithMargin = votesNeeded + (if (doExpensiveSearch) (20:TallyUnscaled) min availableForFudge else 0:TallyUnscaled)

      def tryPrefFrom(preferredCandidateToTransferAwayFrom:Set[CandidateIndex]): Unit = {
        processResult(binarySearchOptimizeVotesNeeded(availableFromOthers,List((c,votesNeededWithMargin)),actuallyDoSearch = true,preferredCandidateToTransferAwayFrom).map{_._1},useATL)
      }

      println("Trying to give quota to candidate "+c+" useATL="+useATL+" use1PRefs="+allow1Prefs+" continuing candidates = "+continuingLowToHigh.mkString(",")+" with tallys "+continuingLowToHigh.map(tallys).mkString(",")+" votesNeeded="+votesNeeded+" votesAvailable="+votesAvailable+" afterStepCount="+afterStepCount)

      // take from anyone
      tryPrefFrom(Set.empty)
      // preferentially take from people who didn't normally win. Less suspicious.
      tryPrefFrom(continuingCandidates.set -- normalElectionOutcome.winners)

    }




    val candidatesToConsiderExcluding : List[CandidateIndex] = if (shouldTryToExcludeEveryoneNotJustInLastRound) continuingCandidates.orderedList.filterNot(candidatesThatWouldNormallyBeExcludedThisRound.contains) else normalElectionOutcome.everyoneInLastRound.filter(continuingCandidates.set)
    println("Starting tamper check, count "+countNumber)
    for (c<-candidatesToConsiderExcluding) { // try to exclude candidate c
      tryToExcludeCandidate(c,useATL = false)
      if (shouldConsiderUsingATL) tryToExcludeCandidate(c,useATL = true)

    }
    // now consider how we could get a (previously losing) candidate a quota.

    val candidatesToConsiderGettingElected : List[CandidateIndex] = continuingCandidates.orderedList.filterNot(normalElectionOutcome.winners.contains)
    for (c<-candidatesToConsiderGettingElected) {
      for (use1Pref<-List(false,true)) {
        tryToGiveQuotaToCandidate(c,useATL = false,allow1Prefs=use1Pref)
        if (shouldConsiderUsingATL) tryToGiveQuotaToCandidate(c,useATL = true,allow1Prefs=use1Pref)
      }
    }




    println("Ending tamper check, count "+countNumber)



  }

}

class MarginAndResults(val margin:Margin,val results:NormalElectionOutcome)

class TamperedVoteNoPapers(val candidateFrom:CandidateIndex,val candidateTo:CandidateIndex,val numVotes:Tally)
