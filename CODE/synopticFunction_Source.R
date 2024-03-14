# Prototype function for the synoptic status algorithm
# design discussion at https://github.com/SOLV-Code/SOS-Synoptic-Status-Algorithm/issues/28

# NOTE: This file creates 2 identical versions of the function:
# synoptic() is the original
# rapid_status() is the new name
# need to keep both around so that the tech report etc works as they are, but
# any new implementation should use the rapid_status()fn call.


synoptic <- function(data.df, algorithm = "Minimalist", id.col = "CU_ID", status.labels = c("Red","RedAmber","Amber","AmberGreen","Green", "None","NotRed"),
                     status.scores = c(5,4,3,2,1,NA,2), group.var = NULL, tracing = FALSE){
# version 0.3
# data.df data file with metric values, with labels in first column (and optionally an IntStatus column)
# algorithm name of an algorithm (for now "Minimalist",  "FancyPants", "CategoricalRealist","SimplyRed","StateOfTheSalmon1","StateOfTheSalmon2",
#  "StateOfTheSalmon3")
# NOTE: for algorithms using simplified status (e.g. "CategoricalRealist"), the modification happens
#       in that part of the code (i.e. RedAmber -> Red, AmberGReen -> Amber)
# id.col column label for unique case identifier (e.g. "CU_ID")
# status.labels labels used for status categories, in the order to be displayed
# status.scores values assigned to each status in status.labels, used to calculate error magnitudes
# group.var column name used for grouping summaries (e.g. by species). default is NULL

require(tidyverse)

data.out <- data.df # start a new data object

scores.df <- data.frame(Status = status.labels,Score = status.scores)


# clean up the int status column (if it exists)
if("IntStatus" %in% names(data.df)){

      data.out$IntStatus[!(data.out$IntStatus %in% status.labels)] <- "None"
}

# ---------------------------------------------------------------------------------------------
# if non-available  algorithm selected
if(!(algorithm %in% c("Minimalist","FancyPants","CategoricalRealist","SimplyRed","StateOfTheSalmon1","StateOfTheSalmon2","StateOfTheSalmon3"))){

	rules.df <- data.frame(Node = NA, Rule = "None")

   warning(paste("Selected algorithm", algorithm, "not yet implemented."))
                 #All statuses assigned NA"))
   stop()

	# REST STILL CRASHES IN SUMMARY BELOW SO PUT IN HARD STOP FOR NOW
   data.out$SynStatus <- "None"
   data.out$BinLabel <- NA
   data.out$BinPath <- NA

   summary.tab <- as.data.frame(matrix(NA,ncol=length(status.labels),dimnames=list("SynStatus", status.labels)))
   summary.tab[,"None"] <- sum(is.na(data.out$SynStatus))



   if("IntStatus" %in% names(data.df)){

  # %>% select(all_of(status.labels)   ))


   # bind_rows not working for some reason, so doing the long way round
   #summary.tab <- bind_rows(as.data.frame(summary.tab),  as.data.frame(as.matrix(table(data.out$IntStatus))) )

   intstatus.summary  <- as.data.frame(t(as.matrix(table(data.out$IntStatus)))   ) %>% select(all_of(status.labels))

   summary.tab <- rbind(summary.tab, intstatus.summary)
   dimnames(summary.tab)[[1]][2] <- "IntStatus"




   confusion.tab <- as.matrix(table(data.out$SynStatus,data.out$IntStatus))
   dimnames(confusion.tab)[[2]] <-  paste0("Int",dimnames(confusion.tab)[[2]])
   dimnames(confusion.tab)[[1]] <-  paste0("Pred",dimnames(confusion.tab)[[1]])
   #matrix(NA,ncol=length(status.labels),nrow=length(status.labels),dimnames=list(paste0("Pred",status.labels), paste0("Int",status.labels)))
   #print(table(data.out$IntStatus,data.out$SynStatus))
   
   confusion.raw.tab <- as.matrix(table(data.out$SynStatus,data.out$IntStatusRaw))
   dimnames(confusion.raw.tab)[[2]] <-  paste0("Int",dimnames(confusion.raw.tab)[[2]])
   dimnames(confusion.raw.tab)[[1]] <-  paste0("Pred",dimnames(confusion.raw.tab)[[1]])
   
   

   }
} # end if non-existent algorithm


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# "MINIMALIST"

if(algorithm == "Minimalist"){
   if(tracing){print(paste("starting", algorithm))}



     node1.df <- data.df

     rules.df <- data.frame(
				Node = paste0("Node", c(3,5,8,9)),
				Status = c("Red", "Red","Green", "Amber"),
				Rule = c("LongTrend < 79", "LongTrend >= 79, then PercChange < -80",
							"LongTrend >= 79, then PercChange >= -80, then LongTrend >= 233" ,
							"LongTrend >= 79, then PercChange >= -80, then LongTrend < 233" )
							)

   # first split: LongTrend at 79

   node1.na <- node1.df %>% dplyr::filter(is.na(LongTrend))
   if(dim(node1.na)[1] > 0){
		node1.na$SynStatus <- "None"
        node1.na$BinLabel <- "Node1.NA"
        node1.na$BinPath <- "No LongTrend, so no SynStatus"
		}

# NODE 2 CALCULATION ----------------------------------------------
	if(dim(node1.df)[1] > 0){
		node2.df <- node1.df %>% dplyr::filter(LongTrend >= 79)
         # not a leaf, so no status yet

		}
	if(dim(node1.df)[1] == 0){
		 node2.df <- node1.df
         # not a leaf, so no status yet
		}
 #print("past 2")

# NODE 3 CALCULATION ----------------------------------------------
	if(dim(node1.df)[1] > 0){
	    #print("node 3-1")
		node3.df <- node1.df %>% dplyr::filter(LongTrend < 79)

		if(dim(node3.df)[1] > 0){
			node3.df$SynStatus <- "Red"
			node3.df$BinLabel <- "Node3"
			node3.df$BinPath <- "LongTrend < 79"
			}
		}

	if(dim(node1.df)[1] == 0){node3.df <- node1.df }

	if(dim(node3.df)[1] == 0){
		node3.df$SynStatus <- character()
        node3.df$BinLabel <- character()
		node3.df$BinPath <- character()
		}

 #print("past 3")

# NODE 4 CALCULATION ----------------------------------------------

	node2.na <- node2.df %>% dplyr::filter(is.na(PercChange))

	if(dim(node2.df)[1] > 0){
	#print("node 4-1")
		node4.df <- node2.df %>% dplyr::filter(PercChange >= -80)
          # not a leaf, so no status yet
			}

    if(dim(node2.na)[1] > 0){
	#print("node 2 na")
		node2.na$SynStatus <- "None"
		node2.na$BinLabel <- "Node2.NA"
		node2.na$BinPath <- "No PercChange, so no SynStatus"
		}

	if(dim(node2.df)[1] == 0){
	#print("node 4-2")
		node4.df <- node2.df
          # not a leaf, so no status yet
		}

 #print("past 4")

# NODE 5 CALCULATION ----------------------------------------------

	if(dim(node2.df)[1] > 0){

		node5.df <- node2.df %>% dplyr::filter(PercChange < -80)

		if(dim(node5.df)[1] > 0){
			node5.df$SynStatus <- "Red"
			node5.df$BinLabel <- "Node5"
			node5.df$BinPath <- "LongTrend >= 79, then PercChange < -80"
		}

		}

	if(dim(node2.df)[1] == 0){node5.df <- node2.df }

	if(dim(node5.df)[1] == 0){
		node5.df$SynStatus <- character()
        node5.df$BinLabel <- character()
        node5.df$BinPath <- character()
		}

 #print("past 5")

# NODE 8 CALCULATION ----------------------------------------------
 	if(dim(node4.df)[1] > 0){
		#print("node8-1")
		node8.df <- node4.df %>% dplyr::filter(LongTrend >= 233)

	if(dim(node8.df)[1] > 0){
	   node8.df$SynStatus <- "Green"
       node8.df$BinLabel <- "Node8"
       node8.df$BinPath <- "LongTrend >= 79, then PercChange >= -80, then LongTrend >= 233"
		}
        #No NA off-ramp needed, because already caught in node1.na
		}

	if(dim(node4.df)[1] == 0){node8.df <- node4.df }

	if(dim(node8.df)[1] == 0){
        node8.df$SynStatus <- character()
        node8.df$BinLabel <- character()
        node8.df$BinPath <- character()
		}

 #print("past 8")


# NODE 9 CALCULATION ----------------------------------------------

 	if(dim(node4.df)[1] > 0){
		node9.df <- node4.df %>% dplyr::filter(LongTrend < 233)
		if(dim(node9.df)[1] > 0){
        	node9.df$SynStatus <- "Amber"
			node9.df$BinLabel <- "Node9"
			node9.df$BinPath <- "LongTrend >= 79, then PercChange >= -80, then LongTrend < 233"
		}
		}

	if(dim(node4.df)[1] == 0){node9.df <- node4.df }
	if(dim(node9.df)[1] == 0){
        node9.df$SynStatus <- character()
        node9.df$BinLabel <- character()
        node9.df$BinPath <- character()
		}
  #print("past 9")

   data.out <- rbind(node1.na,node2.na, node3.df,node5.df,node8.df,node9.df)
    #print(str(data.out))
    #write.csv(data.out,"tmp.csv")
	#print("end minimalist")

} # end Minimalist

#--------------------------------------------------------------------------------------------------------------------------------


# --------------------------------------------------------------------------
# "FANCY PANTS"

if(algorithm == "FancyPants"){
  if(tracing){print(paste("starting", algorithm))}

  node1.df <- data.df

  rules.df <- data.frame(
    Node = paste0("Node", c(3,10,11,9,16,17)),
    Status = c("Red", "Green", "RedAmber", "Red","Amber","AmberGreen"),
    Rule = c("LongTrend < 79",
             "LongTrend >= 79,  then AbsLBM >= 31, then PercChange >= -54" ,
             "LongTrend >= 79,  then AbsLBM >= 31, then PercChange < -54" ,
             "LongTrend >= 79,  then AbsLBM < 31, then RelLBM < 0.88",
             "LongTrend >= 79,  then AbsLBM < 31, then RelLBM < 0.88, then PercChange < 73" ,
             "LongTrend >= 79,  then AbsLBM < 31, then RelLBM < 0.88, then PercChange >= 73" )
  )




 # first split: LongTrend at 79




# NODE 2 CALCULATION ----------------------------------------------

	  node1.na <- node1.df %>% dplyr::filter(is.na(LongTrend))

	if(dim(node1.df)[1] > 0){
	#print("node 2-1")
		 node2.df <- node1.df %>% dplyr::filter(LongTrend >= 79)
		  # not a leaf, so no status yet
		}

    if(dim(node1.na)[1] > 0){
		node1.na$SynStatus <- "None"
		node1.na$BinLabel <- "Node1"
		node1.na$BinPath <- "No LongTrend, so no SynStatus"
		}

	if(dim(node2.df)[1] == 0){
	#print("node2-2")

        node2.df$SynStatus <- character()
        node2.df$BinLabel <- character()
        node2.df$BinPath <- character()
		}

 #print("past 2")






# NODE 3 CALCULATION ----------------------------------------------

	if(dim(node1.df)[1] > 0){
	#print("node 6-1")
		node3.df <- node1.df %>% dplyr::filter(LongTrend < 79)
		if(dim(node3.df)[1] > 0){
				node3.df$SynStatus <- "Red"
				node3.df$BinLabel <- "Node3"
				node3.df$BinPath <- "LongTrend < 79"    }
			}

	if(dim(node3.df)[1] == 0){
	#print("node3-2")
	    node3.df$SynStatus <- character()
        node3.df$BinLabel <- character()
        node3.df$BinPath <- character()
		}

 #print("past 3")


# NODE 4 CALCULATION ----------------------------------------------

	node2.na <- node2.df %>% dplyr::filter(is.na(AbsLBM))

	if(dim(node2.df)[1] > 0){
	#print("node 4-1")
		node4.df <- node2.df %>% dplyr::filter(AbsLBM < 31)
		# not a leaf, so no status yet
		}

    if(dim(node2.na)[1] > 0){
	#print("node 2 na")
		node2.na$SynStatus <- "None"
		node2.na$BinLabel <- "Node2.NA"
		node2.na$BinPath <- "No AbsLBM, so no SynStatus"
		}

	if(dim(node4.df)[1] == 0){
	#print("node4-2")

        node4.df$SynStatus <- character()
        node4.df$BinLabel <- character()
        node4.df$BinPath <- character()
		}

 #print("past 4")





  # NODE 5 CALCULATION ----------------------------------------------


	if(dim(node2.df)[1] > 0){
	#print("node 5-1")
		node5.df <- node2.df %>% dplyr::filter(AbsLBM >= 31)
			# not a leaf, so no status yet
		}


	if(dim(node5.df)[1] == 0){
	#print("node5-2")

        node5.df$SynStatus <- character()
        node5.df$BinLabel <- character()
        node5.df$BinPath <- character()
		}

 #print("past 5")








# NODE 10 CALCULATION ----------------------------------------------

	node5.na <- node5.df %>% dplyr::filter(is.na(PercChange))

	if(dim(node5.df)[1] > 0){
	#print("node 10-1")
		node10.df <- node5.df %>% dplyr::filter(PercChange >= -54)
		if(dim(node10.df)[1] > 0){
				node10.df$SynStatus <- "Green"
				node10.df$BinLabel <- "Node10"
				node10.df$BinPath <- "LongTrend >= 79,  then AbsLBM >= 31, then PercChange >= -54"  }
			}

    if(dim(node5.na)[1] > 0){
	#print("node 3 na")
		node5.na$SynStatus <- "None"
		node5.na$BinLabel <- "Node5.NA"
		node5.na$BinPath <- "No PercChange, so no SynStatus"
		}

	if(dim(node10.df)[1] == 0){
	#print("node10-2")

        node10.df$SynStatus <- character()
        node10.df$BinLabel <- character()
        node10.df$BinPath <- character()
		}

 #print("past 10")






  # NODE 11 CALCULATION ----------------------------------------------


	if(dim(node5.df)[1] > 0){
	#print("node 11-1")
		node11.df <- node5.df %>% dplyr::filter(PercChange < -54)
		if(dim(node11.df)[1] > 0){
				node11.df$SynStatus <- "RedAmber"
				node11.df$BinLabel <- "Node11"
				node11.df$BinPath <- "LongTrend >= 79,  then AbsLBM >= 31, then PercChange <- -54"      }
			}


	if(dim(node11.df)[1] == 0){
	#print("node11-2")
	    node11.df$SynStatus <- character()
        node11.df$BinLabel <- character()
        node11.df$BinPath <- character()
		}

 #print("past 11")











  # NODE 8 CALCULATION ----------------------------------------------

	node4.na <- node4.df %>% dplyr::filter(is.na(RelLBM))

	if(dim(node4.df)[1] > 0){
	#print("node 4-1")
		node8.df <- node4.df %>% dplyr::filter(RelLBM >= 0.88)
		# not a leaf, so no status yet
		}

    if(dim(node4.na)[1] > 0){
	#print("node 4 na")
		node4.na$SynStatus <- "None"
		node4.na$BinLabel <- "Node4.NA"
		node4.na$BinPath <- "No RelLBM, so no SynStatus"
		}

	if(dim(node8.df)[1] == 0){
	#print("node6-2")
	    node8.df$SynStatus <- character()
        node8.df$BinLabel <- character()
        node8.df$BinPath <- character()
		}

 #print("past 8")







# NODE 9 CALCULATION ----------------------------------------------

	if(dim(node4.df)[1] > 0){
	#print("node 9-1")
		node9.df <- node4.df %>% dplyr::filter(RelLBM < 0.88)
		if(dim(node9.df)[1] > 0){
				node9.df$SynStatus <- "Red"
				node9.df$BinLabel <- "Node9"
				node9.df$BinPath <- "LongTrend >= 79,  then AbsLBM < 31, then RelLBM < 0.88"    }
			}


	if(dim(node9.df)[1] == 0){
	#print("node9-2")
	    node9.df$SynStatus <- character()
        node9.df$BinLabel <- character()
        node9.df$BinPath <- character()
		}

 #print("past 9")






# NODE 16 CALCULATION ----------------------------------------------

	node8.na <- node8.df %>% dplyr::filter(is.na(PercChange))

	if(dim(node8.df)[1] > 0){
	#print("node 16-1")
		node16.df <- node8.df %>% dplyr::filter(PercChange >= 73)
		if(dim(node16.df)[1] > 0){
			node16.df$SynStatus <- "AmberGreen"
			node16.df$BinLabel <- "Node16"
			node16.df$BinPath <- "LongTrend >= 79,  then AbsLBM < 31, then RelLBM < 0.88, then PercChange >= 73"   }
			}

    if(dim(node8.na)[1] > 0){
	#print("node 3 na")
		node8.na$SynStatus <- "None"
		node8.na$BinLabel <- "Node8.NA"
		node8.na$BinPath <- "No PercChange, so no SynStatus"
		}

	if(dim(node16.df)[1] == 0){
	#print("node16-2")

        node16.df$SynStatus <- character()
        node16.df$BinLabel <- character()
        node16.df$BinPath <- character()
		}

 #print("past 16")








# NODE 17 CALCULATION ----------------------------------------------



	if(dim(node8.df)[1] > 0){
	#print("node 17-1")
		node17.df <- node8.df %>% dplyr::filter(PercChange < 73)
		if(dim(node17.df)[1] > 0){
				node17.df$SynStatus <- "Amber"
				node17.df$BinLabel <- "Node17"
				node17.df$BinPath <- "LongTrend >= 79,  then AbsLBM < 31, then RelLBM < 0.88, then PercChange < 73"  }
			}


	if(dim(node17.df)[1] == 0){
	#print("node17-2")

        node17.df$SynStatus <- character()
        node17.df$BinLabel <- character()
        node17.df$BinPath <- character()
		}

 #print("past 17")





  data.out <- rbind(node1.na,node2.na,node4.na,node5.na,node8.na,
				node3.df,
				node10.df,node11.df,
				node9.df,node16.df,node17.df)


} # end FancyPants

#--------------------------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------
# "CategoricalRealist"

if(algorithm == "CategoricalRealist"){
  if(tracing){print(paste("starting", algorithm))}

  node1.df <- data.df

  rules.df <- data.frame(
    Node = paste0("Node", c(4,5,6,7)),
    Status = c("Amber","Red","Amber","Red"),
    Rule = c("DataType is Rel_idx, then LongTrend is Green",
             "DataType is Rel_idx, then LongTrend is Red or Amber",
			 "DataType is Abs_Abd, then RelLBM is Amber or Green",
             "DataType is Abs_Abd, then RelLBM is Red"
             )
  )

  # first split: LongTrend at 79
  node1.na <- node1.df %>% dplyr::filter(!(DataType %in% c("Rel_Idx","Abs_Abd")))

  if(dim(node1.na)[1]>0){
    node1.na$SynStatus <- "None"
    node1.na$BinLabel <- "Node1"
    node1.na$BinPath <- "No matching DataType, so no SynStatus"
  }


 # NODE 2 CALCULATION ----------------------------------------------
	if(dim(node1.df)[1] > 0){
		node2.df <- node1.df %>% dplyr::filter(DataType == "Rel_Idx")
         # not a leaf, so no status yet

		}
	if(dim(node1.df)[1] == 0){
		 node2.df <- node1.df
         # not a leaf, so no status yet
		}
 #print("past 2")





# NODE 3 CALCULATION ----------------------------------------------
	if(dim(node1.df)[1] > 0){
	    #print("node 3-1")
		node3.df <- node1.df %>% dplyr::filter(DataType == "Abs_Abd")
		  # not a leaf, so no status yet
		}

	if(dim(node1.df)[1] == 0){node3.df <- node1.df }

 #print("past 3")


# NODE 4 CALCULATION ----------------------------------------------

	node2.na <- node2.df %>% dplyr::filter(is.na(LongTrend))

	if(dim(node2.df)[1] > 0){
	#print("node 4-1")
		node4.df <- node2.df %>% dplyr::filter(LongTrend != "Green")
		node4.df$SynStatus <- "Red"
		node4.df$BinLabel <- "Node4"
		node4.df$BinPath <- "DataType = Rel_idx, then LongTrend = Not Green"
			}

    if(dim(node2.na)[1] > 0){
	#print("node 2 na")
		node2.na$SynStatus <- "None"
		node2.na$BinLabel <- "Node2.NA"
		node2.na$BinPath <- "No LongTrend, so no SynStatus"
		}

	if(dim(node2.df)[1] == 0){
	#print("node 4-2")
		node4.df <- node2.df
        node4.df$SynStatus <- character()
        node4.df$BinLabel <- character()
        node4.df$BinPath <- character()
		}

 #print("past 4")


# NODE 5 CALCULATION ----------------------------------------------

	if(dim(node2.df)[1] > 0){

		node5.df <- node2.df %>% dplyr::filter(LongTrend == "Green" )

		if(dim(node5.df)[1] > 0){
			node5.df$SynStatus <- "Amber"
			node5.df$BinLabel <- "Node5"
			node5.df$BinPath <- "DataType = Rel_idx, then LongTrend = Green"
			}
		}

	if(dim(node2.df)[1] == 0){node5.df <- node2.df }

	if(dim(node5.df)[1] == 0){
		node5.df$SynStatus <- character()
        node5.df$BinLabel <- character()
        node5.df$BinPath <- character()
		}

 #print("past 5")


# NODE 6 CALCULATION ----------------------------------------------

	node3.na <- node3.df %>% dplyr::filter(is.na(RelLBM))

	if(dim(node3.df)[1] > 0){
	#print("node 6-1")
		node6.df <- node3.df %>% dplyr::filter(RelLBM != "Red")
		if(dim(node6.df)[1] > 0){
				node6.df$SynStatus <- "Amber"
				node6.df$BinLabel <- "Node6"
				node6.df$BinPath <- "DataType = Abs_Abd, then RelLBM = Not Red"   }
			}

    if(dim(node3.na)[1] > 0){
	#print("node 3 na")
		node3.na$SynStatus <- "None"
		node3.na$BinLabel <- "Node3.NA"
		node3.na$BinPath <- "No RelLBM, so no SynStatus"
		}

	if(dim(node6.df)[1] == 0){
	#print("node6-2")

        node6.df$SynStatus <- character()
        node6.df$BinLabel <- character()
        node6.df$BinPath <- character()
		}

 #print("past 6")



  # NODE 7 CALCULATION ----------------------------------------------

	if(dim(node3.df)[1] > 0){

		node7.df <- node3.df %>% dplyr::filter(RelLBM == "Red" )

		if(dim(node7.df)[1] > 0){
			node7.df$SynStatus <- "Red"
			node7.df$BinLabel <- "Node7"
			node7.df$BinPath <- "DataType = Abs_Abd, then RelLBM = Red"
			}
		}

	if(dim(node3.df)[1] == 0){node7.df <- node3.df }

	if(dim(node7.df)[1] == 0){
		node7.df$SynStatus <- character()
        node7.df$BinLabel <- character()
        node7.df$BinPath <- character()
		}

 #print("past 7")

 if(FALSE){
 print("node1.df");print(dim(node1.df))
 print("node1.na");print(dim(node1.na))
  print("node2.df");print(dim(node2.df))
 print("node2.na");print(dim(node2.na))
  print("node3.df");print(dim(node3.df))
 print("node3.na");print(dim(node3.na))
 print("node4.df");print(dim(node4.df))
 print("node5.df");print(dim(node5.df))
 print("node6.df");print(dim(node6.df))
print("node7.df"); print(dim(node7.df))
}

  data.out <- rbind(node1.na,node2.na,node3.na,node4.df,node5.df,node6.df,node7.df)



} # end CategoricalRealist

#---------------------------------------------------------------------------------


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# "SimplyRed"

if(algorithm == "SimplyRed"){
  if(tracing){print(paste("starting", algorithm))}

  node1.df <- data.df

  rules.df <- data.frame(
    Node = paste0("Node", c(3,5,8,9)),
    Status = c("Red", "Red","NotRed", "Red"),
    Rule = c("LongTrend < 79", "LongTrend >= 79, then PercChange < -70",
             "LongTrend >= 79, then PercChange >= -70, then RelLBM >= 1" ,
             "LongTrend >= 79, then PercChange >= -70, then RelLBM < 1" )
  )

  # first split: LongTrend at 79
  node1.na <- node1.df %>% dplyr::filter(is.na(LongTrend))
  node1.na$SynStatus <- "None"
  node1.na$BinLabel <- "Node1.NA"
  node1.na$BinPath <- "No LongTrend, so no SynStatus"

# NODE 2 CALCULATION ----------------------------------------------
	if(dim(node1.df)[1] > 0){
		node2.df <- node1.df %>% dplyr::filter(LongTrend >= 79)
         # not a leaf, so no status yet

		}
	if(dim(node1.df)[1] == 0){
		 node2.df <- node1.df
         # not a leaf, so no status yet
		}
 #print("past 2")


# NODE 3 CALCULATION ----------------------------------------------
	if(dim(node1.df)[1] > 0){
	    #print("node 3-1")
		node3.df <- node1.df %>% dplyr::filter(LongTrend < 79)

		if(dim(node3.df)[1] > 0){
			node3.df$SynStatus <- "Red"
			node3.df$BinLabel <- "Node3"
			node3.df$BinPath <- "LongTrend < 79"
			}
		}

	if(dim(node1.df)[1] == 0){node3.df <- node1.df }

	if(dim(node3.df)[1] == 0){
		node3.df$SynStatus <- character()
        node3.df$BinLabel <- character()
		node3.df$BinPath <- character()
		}


# NODE 4 CALCULATION ----------------------------------------------

	node2.na <- node2.df %>% dplyr::filter(is.na(PercChange))

	if(dim(node2.df)[1] > 0){
	#print("node 4-1")
		node4.df <- node2.df %>% dplyr::filter(PercChange >= -70)
          # not a leaf, so no status yet
			}

    if(dim(node2.na)[1] > 0){
	#print("node 2 na")
		node2.na$SynStatus <- "None"
		node2.na$BinLabel <- "Node2.NA"
		node2.na$BinPath <- "No PercChange, so no SynStatus"
		}

	if(dim(node2.df)[1] == 0){
	#print("node 4-2")
		node4.df <- node2.df
          # not a leaf, so no status yet
		}

 #print("past 4")



# NODE 5 CALCULATION ----------------------------------------------

	if(dim(node2.df)[1] > 0){

		node5.df <- node2.df %>% dplyr::filter(PercChange < -70)

		if(dim(node5.df)[1] > 0){
			node5.df$SynStatus <- "Red"
			node5.df$BinLabel <- "Node5"
			node5.df$BinPath <- "LongTrend >= 79, then PercChange < -70"
		}

		}

	if(dim(node2.df)[1] == 0){node5.df <- node2.df }

	if(dim(node5.df)[1] == 0){
		node5.df$SynStatus <- character()
        node5.df$BinLabel <- character()
        node5.df$BinPath <- character()
		}

 #print("past 5")


   # NODE 8 CALCULATION ----------------------------------------------

	node4.na <- node4.df %>% dplyr::filter(is.na(RelLBM))


	if(dim(node4.df)[1] > 0){
		#print("node8-1")
		node8.df <- node4.df %>% dplyr::filter(RelLBM >= 1)

	if(dim(node8.df)[1] > 0){
		node8.df$SynStatus <- "NotRed"
		node8.df$BinLabel <- "Node8"
		node8.df$BinPath <- "LongTrend >= 79, then PercChange >= -70, then RelLBM >= 1"
		}

		}

	if(dim(node4.na)[1] > 0){
	#print("node 4 na")
		node4.na$SynStatus <- "None"
		node4.na$BinLabel <- "Node4.NA"
		node4.na$BinPath <- "No RelLBM, so no SynStatus"
		}


	if(dim(node4.df)[1] == 0){node8.df <- node4.df }

	if(dim(node8.df)[1] == 0){
        node8.df$SynStatus <- character()
        node8.df$BinLabel <- character()
        node8.df$BinPath <- character()
		}

 #print("past 8")



# NODE 9 CALCULATION ----------------------------------------------

 	if(dim(node4.df)[1] > 0){
		node9.df <- node4.df %>% dplyr::filter(RelLBM < 1)
		if(dim(node8.df)[1] > 0){
			node9.df$SynStatus <- "Red"
			node9.df$BinLabel <- "Node9"
			node9.df$BinPath <- "LongTrend >= 79, then PercChange >= -70, then RelLBM < 1"
		}
		}

	if(dim(node4.df)[1] == 0){node9.df <- node4.df }
	if(dim(node9.df)[1] == 0){
        node9.df$SynStatus <- character()
        node9.df$BinLabel <- character()
        node9.df$BinPath <- character()
		}
  #print("past 9")

  #print(names(node1.na))
    #print(names(node2.na))
	  #print(names(node4.na))
  data.out <-  rbind(node1.na,node2.na,node4.na,
					node3.df,node5.df,node8.df,node9.df)

} # end SimplyRed

#--------------------------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------
if(algorithm == "StateOfTheSalmon1"){
  if(tracing){print(paste("starting", algorithm))}



  node1.df <- data.df

  rules.df <- data.frame(
    Node = paste0("Node", c(7,12,13, 5,9,16,17)),
    Status = c("Red","Green","Amber","Red","Red","Green","Amber"),
    Rule = c("Have RelLBM, then RelLBM < 1",
             "Have RelLBM, then RelLBM >= 1, then RelUBM >= 1.1",
             "Have RelLBM, then RelLBM >= 1, then RelUBM < 1.1",
             "Don't have RelLBM, then Data Type is Abs_Abd AND AbsLBM < 1.5",
             "Don't have RelLBM, then Data Type is Rel_idx OR AbsLBM >= 1.5, then LongTrend < 79",
             "Don't have RelLBM, then Data Type is Rel_idx OR AbsLBM >= 1.5, then LongTrend >= 79, then PercChange >= -70",
             "Don't have RelLBM, then Data Type is Rel_idx OR AbsLBM >= 1.5, then LongTrend >= 79, then PercChange < -70"
             )

    )

  # Don't need a node1.na, because the node already checks whether have/have not


  # first split: Have RelLBM
  node2.df <- node1.df %>% dplyr::filter(is.na(RelLBM))
  # not a leaf, so no status yet
  # don't need NA or empty df off-ramps yet

  node3.df <- node1.df %>% dplyr::filter(!is.na(RelLBM))
  # not a leaf, so no status yet
    # don't need NA or empty df off-ramps yet




  # not a leaf, so no status yet

# NODE 6 CALCULATION ----------------------------------------------

	# don't need NA off-ramp yet: only end up in this spot
	# if have RelLBM

	if(dim(node3.df)[1] > 0){
	#print("node 6-1")
		node6.df <- node3.df %>% dplyr::filter(RelLBM >= 1)
          # not a leaf, so no status yet
			}

	if(dim(node3.df)[1] == 0){
	#print("node 4-2")
		node6.df <- node3.df
          # not a leaf, so no status yet
		}

 #print("past 6")



 # NODE 7 CALCULATION ----------------------------------------------

	if(dim(node3.df)[1] > 0){
	#print("node 7-1")
		node7.df <- node3.df %>% dplyr::filter(RelLBM < 1)


	if(dim(node7.df)[1] > 0){
		node7.df$SynStatus <- "Red"
		node7.df$BinLabel <- "Node7"
		node7.df$BinPath <- "Have RelLBM, then RelLBM < 1"
		}


		}

	if(dim(node3.df)[1] == 0){node7.df <- node3.df }
	#print("node 7-2")

	if(dim(node7.df)[1] == 0){
        node7.df$SynStatus <- character()
        node7.df$BinLabel <- character()
        node7.df$BinPath <- character()
		}

 #print("past 7")



  # NODE 12 CALCULATION ----------------------------------------------

	# just in case there is a lower BM to get here, but no upper BM for the next step
	node6.na <- node3.df %>% dplyr::filter(is.na(RelUBM))

	if(dim(node6.df)[1] > 0){
	#print("node 12-1")
		node12.df <- node6.df %>% dplyr::filter(RelUBM >= 1.1)

		if(dim(node12.df)[1] > 0){
			node12.df$SynStatus <- "Green"
			node12.df$BinLabel <- "Node12"
			node12.df$BinPath <- "Have RelLBM, then RelLBM > 1, then RelUBM >= 1.1"
			}
		}

    if(dim(node6.na)[1] > 0){
	#print("node 6 na")
		node6.na$SynStatus <- "None"
		node6.na$BinLabel <- "Node6.NA"
		node6.na$BinPath <- "No RelUBM, so no SynStatus"
		}

	if(dim(node6.df)[1] == 0){node12.df <- node6.df }


	if(dim(node12.df)[1] == 0){
	#print("node 12-2")
        node12.df$SynStatus <- character()
        node12.df$BinLabel <- character()
        node12.df$BinPath <- character()
		}

 #print("past 12")


# NODE 13 CALCULATION ----------------------------------------------


	if(dim(node6.df)[1] > 0){
	#print("node 13-1")
		 node13.df <- node6.df %>% dplyr::filter(RelUBM < 1.1)

		if(dim(node13.df)[1] > 0){
				node13.df$SynStatus <- "Amber"
				node13.df$BinLabel <- "Node13"
				node13.df$BinPath <- "Have RelLBM, then RelLBM > 1, then RelUBM < 1.1"
			}
		}


	if(dim(node6.df)[1] == 0){node13.df <- node6.df }


	if(dim(node13.df)[1] == 0){
	#print("node 13-2")
        node13.df$SynStatus <- character()
        node13.df$BinLabel <- character()
        node13.df$BinPath <- character()
		}

 #print("past 13")





# NODE 4 CALCULATION ----------------------------------------------

  #node2.na <- node1.df %>% dplyr::filter(is.na(RelLBM) & (is.na(AbsLBM) | is.na(DataType)))
  #node2.na <- node1.df %>% dplyr::filter(is.na(DataType))
  #write.csv(node2.na,"test_node2na.csv")
  #print("-----")
  node2.na <- node1.df %>% dplyr::filter(is.na(DataType))
  #print("-----")
  #print(node2.na) # print(gives the correct empty df
  #write.csv(node2.na,"test_node2na.csv")   # BUT this outputs an incorrect df????????????????????????????



  if(dim(node2.df)[1] > 0){
    #print("node 2-1")
    #node4.df <- node2.df %>% dplyr::filter(is.na(RelLBM) &  !(is.na(AbsLBM) | is.na(DataType)) &
    #											!(AbsLBM < 1.5 & DataType == "Abs_Abd") )
    node4.df <- node2.df %>% dplyr::filter( !(AbsLBM < 1.5 & DataType == "Abs_Abd") )
    # not a leaf, so no status yet
  }

	 if(dim(node2.na)[1] > 0){
	#print("node 2 na")
		node2.na$SynStatus <- "None"
		node2.na$BinLabel <- "Node2.NA"
		node2.na$BinPath <- "No AbsLBM or no DataType, so no SynStatus"
		}


	if(dim(node2.df)[1] == 0){node4.df <- node2.df }

			# not a leaf, so no status yet

 #print("past 4")

  #write.csv(node4.df,"test_node4df.csv")

 # NODE 5 CALCULATION ----------------------------------------------


	if(dim(node2.df)[1] > 0){
	#print("node 5-1")
		 node5.df <- node2.df %>% dplyr::filter(AbsLBM < 1.5 & DataType == "Abs_Abd")

		if(dim(node5.df)[1] > 0){
				node5.df$SynStatus <- "Red"
				node5.df$BinLabel <- "Node5"
				node5.df$BinPath <- "Don't have RelLBM, then Data Type is AbsAbd AND AbsLBM < 1.5"
			}
		}


	if(dim(node2.df)[1] == 0){node5.df <- node2.df }


	if(dim(node5.df)[1] == 0){
	#print("node 5-2")
        node5.df$SynStatus <- character()
        node5.df$BinLabel <- character()
        node5.df$BinPath <- character()
		}

 #print("past 5")




# NODE 8 CALCULATION ----------------------------------------------

	# WHY DOES THIS ONE HAVE TO BE DIFFERENT?
	# node4.df vs. node2.df?
  node4.na <- node4.df %>% dplyr::filter(is.na(LongTrend) & !is.na(PercChange))

	if(dim(node4.df)[1] > 0){
	#print("node 8-1")
		node8.df <- node4.df %>% dplyr::filter(LongTrend >= 79)

		# not a leaf, so no status yet
		}

    if(dim(node4.na)[1] > 0){
	#print("node 6 na")
		node4.na$SynStatus <- "None"
		node4.na$BinLabel <- "Node4.NA"
		node4.na$BinPath <- "No LongTrend, so no SynStatus"
		}

	if(dim(node4.df)[1] == 0){node8.df <- node4.df }


	if(dim(node8.df)[1] == 0){
	#print("node 8-2")
        node8.df$SynStatus <- character()
        node8.df$BinLabel <- character()
        node8.df$BinPath <- character()
		}

 #print("past 8")




  # NODE 9 CALCULATION ----------------------------------------------

#print("starting 9")

	if(dim(node4.df)[1] > 0){
	#print("node 9-1")
		 node9.df <- node4.df %>% dplyr::filter(LongTrend < 79)

		if(dim(node9.df)[1] > 0){
				  node9.df$SynStatus <- "Red"
				  node9.df$BinLabel <- "Node9"
				  node9.df$BinPath <- "Don't have RelLBM, then AbsLBM < 1.5, then LongTrend < 79"
			}
		}


	if(dim(node4.df)[1] == 0){node9.df <- node4.df }


	if(dim(node9.df)[1] == 0){
	#print("node 13-2")
        node9.df$SynStatus <- character()
        node9.df$BinLabel <- character()
        node9.df$BinPath <- character()
		}

#print("past 9")




# NODE 16 CALCULATION ----------------------------------------------

	# just in case there is a lower BM to get here, but no upper BM for the next step
	node8.na <- node4.df %>% dplyr::filter(LongTrend > 79 & is.na(PercChange))

	if(dim(node8.df)[1] > 0){
	#print("node 16-1")
		node16.df <- node8.df %>% dplyr::filter(PercChange >= -70)

		if(dim(node16.df)[1] > 0){
				node16.df$SynStatus <- "Green"
				node16.df$BinLabel <- "Node16"
				node16.df$BinPath <- "Don't have RelLBM, then AbsLBM < 1.5, then LongTrend > 79, then PercChange >= -70"
			}
		}

    if(dim(node8.na)[1] > 0){
	#print("node 8 na")
		node8.na$SynStatus <- "None"
		node8.na$BinLabel <- "Node8.NA"
		node8.na$BinPath <- "NoPercChange, so no SynStatus"
		}

	if(dim(node8.df)[1] == 0){node16.df <- node8.df }


	if(dim(node16.df)[1] == 0){
	#print("node 16-2")
        node16.df$SynStatus <- character()
        node16.df$BinLabel <- character()
        node16.df$BinPath <- character()
		}

 #print("past 16")



    # NODE 17 CALCULATION ----------------------------------------------

	if(dim(node8.df)[1] > 0){
	#print("node 17-1")
		 node17.df <- node8.df %>% dplyr::filter(PercChange < -70)

		if(dim(node17.df)[1] > 0){
				    node17.df$SynStatus <- "Amber"
					node17.df$BinLabel <- "Node17"
					node17.df$BinPath <- "Don't have RelLBM, then AbsLBM < 1.5, then LongTrend > 79, then PercChange < -70"
			}
		}

	if(dim(node8.df)[1] == 0){node17.df <- node8.df }


	if(dim(node17.df)[1] == 0){
	#print("node 17-2")
        node17.df$SynStatus <- character()
        node17.df$BinLabel <- character()
        node17.df$BinPath <- character()
		}

 #print("past 17")

  #--------------
  if(FALSE){
 print("starting merge")

  print("node1.df");print(dim(node1.df))

  print("node3.df");print(dim(node3.df))
  print("node6.df");print(dim(node6.df))
  print("node6.na");print(dim(node6.na))
  print("node7.df");print(dim(node7.df))
  print("node12.df");print(dim(node12.df))
  print("node13.df");print(dim(node13.df ))

    print("node2.df");print(dim(node2.df))
  print("node2.na");print(dim(node2.na))
    print("node4.df");print(dim(node4.df))
  print("node4.na");print(dim(node4.na))
    print("node5.df");print(dim(node5.df))
    print("node8.df");print(dim(node8.df))
  print("node8.na");print(dim(node8.na))

  print("node9.df");print(dim(node9.df))
  print("node16.df");print(dim(node16.df))
  print("node17.df");print(dim(node17.df))
  }

  data.out <- rbind(node6.na, node2.na,node4.na,node8.na,
  node7.df,node12.df,node13.df,
  node5.df,node9.df,node16.df,node17.df)




} # end StateOfTheSalmon1



#--------------------------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------
if(algorithm == "StateOfTheSalmon2"){
  if(tracing){print(paste("starting", algorithm))}



  node1.df <- data.df

  rules.df <- data.frame(
    Node = paste0("Node", c(7,12,13, 5,9,16,17)),
    Status = c("Red","Green","Amber","Red","Red","Green","Amber"),
    Rule = c("Have RelBM, then RelBM is Red",
             "Have RelBM, then RelBM is Amber or Green, then RelBM is Green",
             "Have RelBM, then RelBM is Amber or Green, then RelBM is Amber",
             "Don't have RelBM, then Data Type is AbsAbd and AbsBM is Red",
             "Don't have RelBM, then Data Type is Rel_idx OR AbsBM is Amber or Green, then LongTrend is Red or Amber",
             "Don't have RelBM, then Data Type is Rel_idx OR AbsBM is Amber or Green, then LongTrend is Green, then PercChange is Amber or Green",
             "Don't have RelBM, then Data Type is Rel_idx OR AbsBM is Amber or Green, then LongTrend is Green, then PercChange is Red"
    )

  )

  # Don't need a node1.na, because the node already checks whether have/have not


  # first split: Have RelLBM
  node2.df <- node1.df %>% dplyr::filter(is.na(RelLBM))
  # not a leaf, so no status yet
  # don't need NA or empty df off-ramps yet

  node3.df <- node1.df %>% dplyr::filter(!is.na(RelLBM))
  # not a leaf, so no status yet
    # don't need NA or empty df off-ramps yet




  # not a leaf, so no status yet

# NODE 6 CALCULATION ----------------------------------------------

	# don't need NA off-ramp yet: only end up in this spot
	# if have RelLBM

	if(dim(node3.df)[1] > 0){
	#print("node 6-1")
		node6.df <- node3.df %>% dplyr::filter(RelLBM != "Red")
          # not a leaf, so no status yet
			}

	if(dim(node3.df)[1] == 0){
	#print("node 4-2")
		node6.df <- node3.df
          # not a leaf, so no status yet
		}

 #print("past 6")



 # NODE 7 CALCULATION ----------------------------------------------

	if(dim(node3.df)[1] > 0){
	#print("node 7-1")
		node7.df <- node3.df %>% dplyr::filter(RelLBM == "Red")

		if(dim(node7.df)[1] > 0){
		node7.df$SynStatus <- "Red"
		node7.df$BinLabel <- "Node7"
		node7.df$BinPath <- "Have RelLBM, then RelLBM is Red"
		}

		}

	if(dim(node3.df)[1] == 0){node7.df <- node3.df }
	#print("node 7-2")

	if(dim(node7.df)[1] == 0){
        node7.df$SynStatus <- character()
        node7.df$BinLabel <- character()
        node7.df$BinPath <- character()
		}

 #print("past 7")



  # NODE 12 CALCULATION ----------------------------------------------

	# just in case there is a lower BM to get here, but no upper BM for the next step
	node6.na <- node3.df %>% dplyr::filter(is.na(RelUBM))

	if(dim(node6.df)[1] > 0){
	#print("node 12-1")
		node12.df <- node6.df %>% dplyr::filter(RelUBM == "Green")

		if(dim(node12.df)[1] > 0){
			node12.df$SynStatus <- "Green"
			node12.df$BinLabel <- "Node12"
			node12.df$BinPath <- "Have RelLBM, then RelLBM > 1, then RelUBM is Green"
			}
		}

    if(dim(node6.na)[1] > 0){
	#print("node 6 na")
		node6.na$SynStatus <- "None"
		node6.na$BinLabel <- "Node6.NA"
		node6.na$BinPath <- "No RelUBM, so no SynStatus"
		}

	if(dim(node6.df)[1] == 0){node12.df <- node6.df }


	if(dim(node12.df)[1] == 0){
	#print("node 12-2")
        node12.df$SynStatus <- character()
        node12.df$BinLabel <- character()
        node12.df$BinPath <- character()
		}

 #print("past 12")


# NODE 13 CALCULATION ----------------------------------------------


	if(dim(node6.df)[1] > 0){
	#print("node 13-1")
		 node13.df <- node6.df %>% dplyr::filter(RelUBM == "Amber")

		if(dim(node13.df)[1] > 0){
				node13.df$SynStatus <- "Amber"
				node13.df$BinLabel <- "Node13"
				node13.df$BinPath <- "Have RelBM, then RelBM is Not Red, then RelBM is Amber"
			}
		}


	if(dim(node6.df)[1] == 0){node13.df <- node6.df }


	if(dim(node13.df)[1] == 0){
	#print("node 13-2")
        node13.df$SynStatus <- character()
        node13.df$BinLabel <- character()
        node13.df$BinPath <- character()
		}

 #print("past 13")





# NODE 4 CALCULATION ----------------------------------------------

  #node2.na <- node1.df %>% dplyr::filter( (is.na(RelLBM) & is.na(AbsLBM)) | is.na(DataType) )
  #node2.na <- node1.df %>% dplyr::filter(is.na(DataType)) # & is.na(AbsLBM) )
  #print("-----")
  node2.na <- node1.df %>% dplyr::filter(is.na(DataType))
  #print("-----")
  #print(node2.na) # print(gives the correct empty df


  if(dim(node2.df)[1] > 0){
    #print("node 2-1")
    #node4.df <- node2.df %>% dplyr::filter(is.na(RelLBM) &  !(is.na(AbsLBM) | is.na(DataType)) &
    #											!(AbsLBM < 1.5 & DataType == "Abs_Abd") )
    node4.df <- node2.df %>% dplyr::filter( !(AbsLBM == "Red" & DataType == "Abs_Abd") )

    # not a leaf, so no status yet
  }


	 if(dim(node2.na)[1] > 0){
	#print("node 2 na")
		node2.na$SynStatus <- "None"
		node2.na$BinLabel <- "Node2.NA"
		node2.na$BinPath <- "No AbsLBM or no DataType, so no SynStatus"
		}


	if(dim(node2.df)[1] == 0){node4.df <- node2.df }

			# not a leaf, so no status yet

 #print("past 4")


 # NODE 5 CALCULATION ----------------------------------------------


	if(dim(node2.df)[1] > 0){
	#print("node 5-1")
		 node5.df <- node2.df %>% dplyr::filter(AbsLBM == "Red" & DataType == "Abs_Abd")

		if(dim(node5.df)[1] > 0){
				node5.df$SynStatus <- "Red"
				node5.df$BinLabel <- "Node5"
				node5.df$BinPath <- "Don't have RelLBM, then Data Type is AbsAbd AND AbsLBM is Red"
			}
		}


	if(dim(node2.df)[1] == 0){node5.df <- node2.df }


	if(dim(node5.df)[1] == 0){
	#print("node 5-2")
        node5.df$SynStatus <- character()
        node5.df$BinLabel <- character()
        node5.df$BinPath <- character()
		}

 #print("past 5")




# NODE 8 CALCULATION ----------------------------------------------

	#old
	#node4.na <- node2.df %>% dplyr::filter(is.na(LongTrend))
	# WHY DOES THIS ONE HAVE TO BE DIFFERENT?
	# node4.df vs. node2.df?
	node4.na <- node4.df %>% dplyr::filter(is.na(LongTrend) & !is.na(PercChange))
  #print(node4.na)

	if(dim(node4.df)[1] > 0){
	#print("node 8-1")
		node8.df <- node4.df %>% dplyr::filter(LongTrend == "Green")

		# not a leaf, so no status yet
		}

    if(dim(node4.na)[1] > 0){
	#print("node 6 na")
		node4.na$SynStatus <- "None"
		node4.na$BinLabel <- "Node4.NA"
		node4.na$BinPath <- "No LongTrend, so no SynStatus"
		}

	if(dim(node4.df)[1] == 0){node8.df <- node4.df }


	if(dim(node8.df)[1] == 0){
	#print("node 8-2")
        node8.df$SynStatus <- character()
        node8.df$BinLabel <- character()
        node8.df$BinPath <- character()
		}

 #print("past 8")




  # NODE 9 CALCULATION ----------------------------------------------

#print("starting 9")

	if(dim(node4.df)[1] > 0){
	#print("node 9-1")
		 node9.df <- node4.df %>% dplyr::filter(LongTrend %in% c("Red","Amber"))

		if(dim(node9.df)[1] > 0){
				  node9.df$SynStatus <- "Red"
				  node9.df$BinLabel <- "Node9"
				  node9.df$BinPath <- "Don't have RelBM, then AbsBM is Amber or Green, then LongTrend is Red or Amber"
			}
		}


	if(dim(node4.df)[1] == 0){node9.df <- node4.df }


	if(dim(node9.df)[1] == 0){
	#print("node 13-2")
        node9.df$SynStatus <- character()
        node9.df$BinLabel <- character()
        node9.df$BinPath <- character()
		}

#print("past 9")




# NODE 16 CALCULATION ----------------------------------------------

	# just in case there is a lower BM to get here, but no upper BM for the next step
	node8.na <- node4.df %>% dplyr::filter(is.na(PercChange) & is.na(LongTrend) )
  #print(node8.na)

	if(dim(node8.df)[1] > 0){
	#print("node 16-1")
		node16.df <- node8.df %>% dplyr::filter(PercChange %in% c("Amber","Green"))

		if(dim(node16.df)[1] > 0){
				node16.df$SynStatus <- "Green"
				node16.df$BinLabel <- "Node16"
				node16.df$BinPath <- "Don't have RelBM, then AbsBM is Amber or Green, then LongTrend is Green, then PercChange is Amber or Green"
			}
		}

    if(dim(node8.na)[1] > 0){
	#print("node 8 na")
		node8.na$SynStatus <- "None"
		node8.na$BinLabel <- "Node8.NA"
		node8.na$BinPath <- "NoPercChange, so no SynStatus"
		}

	if(dim(node8.df)[1] == 0){node16.df <- node8.df }


	if(dim(node16.df)[1] == 0){
	#print("node 16-2")
        node16.df$SynStatus <- character()
        node16.df$BinLabel <- character()
        node16.df$BinPath <- character()
		}

 #print("past 16")



    # NODE 17 CALCULATION ----------------------------------------------

	if(dim(node8.df)[1] > 0){
	#print("node 17-1")
		 node17.df <- node8.df %>% dplyr::filter(PercChange == "Red")

		if(dim(node17.df)[1] > 0){
				    node17.df$SynStatus <- "Amber"
					node17.df$BinLabel <- "Node17"
					node17.df$BinPath <- "Don't have RelBM, then AbsBM is Amber or Green, then LongTrend is Green, then PercChange is Red"
			}
		}

	if(dim(node8.df)[1] == 0){node17.df <- node8.df }


	if(dim(node17.df)[1] == 0){
	#print("node 17-2")
        node17.df$SynStatus <- character()
        node17.df$BinLabel <- character()
        node17.df$BinPath <- character()
		}

 #print("past 17")

  #--------------
 #print("starting merge")

  #print(dim(node6.na))
  #print(dim(node2.na))
  #print(dim(node4.na))
  #print(dim(node8.na))
  #print(dim(node7.df))
  #print(dim(node12.df))
  #print(dim(node13.df ))
  #print(dim(node5.df))
  #print(dim(node9.df))
  #print(dim(node16.df))
  #print(dim(node17.df))

  data.out <- rbind(node6.na, node2.na,node4.na,
                    node8.na,
  node7.df,node12.df,node13.df,
  node5.df,node9.df,node16.df,node17.df)




} # end StateOfTheSalmon2




#--------------------------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------
if(algorithm == "StateOfTheSalmon3"){
  if(tracing){print(paste("starting", algorithm))}

rules.df <- data.frame(
    Node = paste0("Node", c(3,17,19,20,21,22,23,33,36,37,64,65)),
    Status = c("Red","Red","Red","Amber","Red","Amber","Red","Red","Green","Amber","Green","Amber"),
    Rule = c("Data Type is AbsAbd AND AbsLBM < 1.5",
             "Data Type is RelIdx OR AbsLBM >= 1.5, then Data Type is RelIdx OR AbsUBM >= 1, then Don't have RelLBM, then LongTrend < 79",
             "Data Type is RelIdx OR AbsLBM >= 1.5, then Data Type is RelIdx OR AbsUBM >= 1, then Have RelLBM, then RelLBM < 1",
             "Data Type is RelIdx OR AbsLBM >= 1.5, then Data Type is AbsAbd AND AbsUBM < 1,then Don't have RelLBM, then LongTrend >= 79",
             "Data Type is RelIdx OR AbsLBM >= 1.5, then Data Type is AbsAbd AND AbsUBM < 1,then Don't have RelLBM, then LongTrend < 79" ,
             "Data Type is RelIdx OR AbsLBM >= 1.5, then Data Type is AbsAbd AND AbsUBM < 1,then Have RelLBM, then RelLBM >= 1",
             "Data Type is RelIdx OR AbsLBM >= 1.5, then Data Type is AbsAbd AND AbsUBM < 1,then Have RelLBM, then RelLBM < 1",
             "Data Type is RelIdx OR AbsLBM >= 1.5, then Data Type is RelIdx OR AbsUBM >= 1, then Don't have RelLBM, then LongTrend >= 79, then PercChange < -70",
             "Data Type is RelIdx OR AbsLBM >= 1.5, then Data Type is RelIdx OR AbsUBM >= 1, then Have RelLBM, then RelLBM >= 1, then RelUBM >= 1.1",
             "Data Type is RelIdx OR AbsLBM >= 1.5, then Data Type is RelIdx OR AbsUBM >= 1, then Have RelLBM, then RelLBM >= 1, then RelUBM < 1.1",
             "Data Type is RelIdx OR AbsLBM >= 1.5, then Data Type is RelIdx OR AbsUBM >= 1, then Don't have RelLBM, then LongTrend >= 79, then PercChange >= -70, then LongTrend >= 233",
             "Data Type is RelIdx OR AbsLBM >= 1.5, then Data Type is RelIdx OR AbsUBM >= 1, then Don't have RelLBM, then LongTrend >= 79, then PercChange >= -70, then LongTrend < 233"
    ))


# NODE 1 ---------------------------------------

     node1.df <- data.df


# NODES 2 & 3 ---------------------------------------------------------

#write.csv(node1.df,"test_node1.csv")


node2.df <- node1.df %>% dplyr::filter( DataType == "Rel_Idx" | is.na(AbsLBM) | AbsLBM >= 1.5 )
# not a leaf, so no status yet
if(dim(node2.df)[1] == 0){
  node2.df$SynStatus <- character()
  node2.df$BinLabel <- character()
  node2.df$BinPath <- character()
}
#write.csv(node2.df,"test_node2.csv")

node2.na <- node1.df %>% dplyr::filter(is.na(DataType))
if(dim(node2.na)[1] > 0){
  node2.na$SynStatus <- "None"
  node2.na$BinLabel <- "Node2.NA"
  node2.na$BinPath <- "No AbsLBM or no DataType, so no SynStatus"
  #write.csv(node2.na,"test_node2na.csv")
}


node3.df <- node1.df %>% dplyr::filter( DataType == "Abs_Abd" & AbsLBM < 1.5 )
if(dim(node3.df)[1] > 0){
  node3.df$SynStatus <- "Red"
  node3.df$BinLabel <- "Node3"
  node3.df$BinPath <- "Data Type is AbsAbd AND AbsLBM < 1.5"
  #write.csv(node3.df,"test_node3.csv")
}



# Nodes 4 & 5 (ABS UBM < 1) -----------------------------------------------------


node4.df <- node2.df %>% dplyr::filter( DataType == "Rel_Idx" | is.na(AbsUBM) | AbsUBM >= 1)
# not a leaf, so no status yet
if(dim(node4.df)[1] == 0){
  node4.df$SynStatus <- character()
  node4.df$BinLabel <- character()
  node4.df$BinPath <- character()
}
#write.csv(node4.df,"test_node4.csv")

# node4.na NOT NEEDED


node5.df <- node2.df %>% dplyr::filter( DataType == "Abs_Abd" & AbsUBM < 1)
# not a leaf, so no status yet
if(dim(node5.df)[1] == 0){
  node5.df$SynStatus <- character()
  node5.df$BinLabel <- character()
  node5.df$BinPath <- character()
}
#write.csv(node5.df,"test_node5.csv")





# NODES SUB-5 - HAVE REL BM ----------------------------------------------------


node11.df <- node5.df  %>% dplyr::filter(!is.na(RelLBM))
# not a leaf, so no status yet
if(dim(node11.df)[1] == 0){
  node11.df$SynStatus <- character()
  node11.df$BinLabel <- character()
  node11.df$BinPath <- character()
}
#write.csv(node11.df,"test_node11.csv")

# node11.NA not needed b/c checking for RelLBM = NA already



node22.df <- node11.df %>% dplyr::filter(RelLBM >=1)
if(dim(node22.df)[1] > 0){
  node22.df$SynStatus <- "Amber"
  node22.df$BinLabel <- "Node22"
  node22.df$BinPath <- "Data Type is RelAbd OR AbsLBM >= 1.5, then Data Type is AbsAbd AND AbsUBM < 1,then Have RelLBM, then RelLBM >= 1"
  #write.csv(node22.df,"test_node22.csv")
}


node23.df <- node11.df %>% dplyr::filter(RelLBM < 1)
if(dim(node23.df)[1] > 0){
  node23.df$SynStatus <- "Red"
  node23.df$BinLabel <- "Node23"
  node23.df$BinPath <- "Data Type is RelAbd OR AbsLBM >= 1.5, then Data Type is AbsAbd AND AbsUBM < 1,then Have RelLBM, then RelLBM < 1"
  #write.csv(node23.df,"test_node23.csv")
}



# NODES SUB-5 - DO NOT HAVE REL BM -----------------------------------


node10.df <- node5.df  %>% dplyr::filter(is.na(RelLBM))
# not a leaf, so no status yet
if(dim(node10.df)[1] == 0){
  node10.df$SynStatus <- character()
  node10.df$BinLabel <- character()
  node10.df$BinPath <- character()
}
#write.csv(node10.df,"test_node10.csv")

# Node 10 NA not needed b/c checking for NA already

node20.df <- node10.df %>% dplyr::filter(LongTrend >= 79)
if(dim(node20.df)[1] > 0){
  node20.df$SynStatus <- "Amber"
  node20.df$BinLabel <- "Node20"
  node20.df$BinPath <- "Data Type is RelAbd OR AbsLBM >= 1.5, then Data Type is AbsAbd AND AbsUBM < 1,then Don't have RelLBM, then LongTrend >= 79"
  #write.csv(node20.df,"test_node20.csv")
}


node21.df <- node10.df %>% dplyr::filter(LongTrend < 79)
if(dim(node21.df)[1] > 0){
  node21.df$SynStatus <- "Red"
  node21.df$BinLabel <- "Node21"
  node21.df$BinPath <- "Data Type is RelAbd OR AbsLBM >= 1.5, then Data Type is AbsAbd AND AbsUBM < 1,then Have RelLBM, then RelLBM < 1"
  #write.csv(node21.df,"test_node21.csv")
}




# NODES SUB-4 - HAVE REL BM


node9.df <- node4.df  %>% dplyr::filter(!is.na(RelLBM))
# not a leaf, so no status yet
if(dim(node9.df)[1] == 0){
  node9.df$SynStatus <- character()
  node9.df$BinLabel <- character()
  node9.df$BinPath <- character()
}
#write.csv(node9.df,"test_node9.csv")


# Node9NA not needed b/c checking for NA already

node19.df <- node9.df %>% dplyr::filter(RelLBM < 1)
if(dim(node19.df)[1] > 0){
  node19.df$SynStatus <- "Red"
  node19.df$BinLabel <- "Node19"
  node19.df$BinPath <- "Data Type is RelAbd OR AbsLBM >= 1.5, then Data Type is RelAbd OR AbsUBM >= 1, then Have RelLBM, then RelLBM < 1"
  #write.csv(node19.df,"test_node19.csv")
}


node18.df <- node9.df %>% dplyr::filter(RelLBM >=1)
# not a leaf, so no status yet
if(dim(node18.df)[1] == 0){
  node18.df$SynStatus <- character()
  node18.df$BinLabel <- character()
  node18.df$BinPath <- character()
}
#write.csv(node18.df,"test_node18.csv")

#Node 18NA not needed b/c checking for NA in node 9 already

node36.df <- node18.df %>% dplyr::filter(RelUBM >= 1.1)
if(dim(node36.df)[1] > 0){
  node36.df$SynStatus <- "Green"
  node36.df$BinLabel <- "Node36"
  node36.df$BinPath <- "Data Type is RelAbd OR AbsLBM >= 1.5, then Data Type is RelAbd OR AbsUBM >= 1, then Have RelLBM, then RelLBM >= 1, then RelUBM >= 1.1"
  #write.csv(node36.df,"test_node36.csv")
}


node37.df <- node18.df %>% dplyr::filter(RelUBM < 1.1)
if(dim(node37.df)[1] > 0){
  node37.df$SynStatus <- "Amber"
  node37.df$BinLabel <- "Node37"
  node37.df$BinPath <- "Data Type is RelAbd OR AbsLBM >= 1.5, then Data Type is RelAbd OR AbsUBM >= 1, then Have RelLBM, then RelLBM >= 1, then RelUBM < 1.1"
  #write.csv(node37.df,"test_node37.csv")
}







# NODES SUB-4 - DO NOT HAVE REL BM


node8.df <- node4.df  %>% dplyr::filter(is.na(RelLBM))
# not a leaf, so no status yet
if(dim(node8.df)[1] == 0){
  node8.df$SynStatus <- character()
  node8.df$BinLabel <- character()
  node8.df$BinPath <- character()
}
#write.csv(node8.df,"test_node8.csv")



# Node8 NA not needed, already checking for RelLBM  = NA

node17.df <- node8.df %>% dplyr::filter(LongTrend < 79)
if(dim(node17.df)[1] > 0){
  node17.df$SynStatus <- "Red"
  node17.df$BinLabel <- "Node17"
  node17.df$BinPath <- "Data Type is RelAbd OR AbsLBM >= 1.5, then Data Type is RelAbd OR AbsUBM >= 1, then Don't have RelLBM, then LongTrend < 79"
  #write.csv(node17.df,"test_node17.csv")
}


# just in case there is a info here, but no LongTrend for the next step
node16.na <- node8.df %>% dplyr::filter(is.na(LongTrend))
if(dim(node16.na)[1] > 0){
  #print("node 16 na")
  node16.na$SynStatus <- "None"
  node16.na$BinLabel <- "Node16.NA"
  node16.na$BinPath <- "No LongTrend, so no SynStatus"
  #write.csv(node16.na,"test_node16NA.csv")
}

node16.df <- node8.df %>% dplyr::filter(LongTrend >= 79)
# not a leaf, so no status yet
if(dim(node16.df)[1] == 0){
  node16.df$SynStatus <- character()
  node16.df$BinLabel <- character()
  node16.df$BinPath <- character()
}
#write.csv(node16.df,"test_node16.csv")

# COMMENTED OUT MARCH 2024
#NEW May 2023 For cases where have long trend but no short trend
#node32.na <- node16.df %>% dplyr::filter(is.na(PercChange))
# ALSO REMOVED node32.na FROM THE OUTPUT OBJECT AT THE END
#if(dim(node32.na)[1] > 0){
  #print("node 32 na")
#  node32.na$SynStatus <- "None"
#  node32.na$BinLabel <- "Node32.NA"
#  node32.na$BinPath <- "LongTrend > 79, but no ShortTrend, so no SynStatus"
  #write.csv(node16.na,"test_node16NA.csv")
#}

# REVISED MAR 2014 TO DEAL WITH FRASER CK CUs
# CASES WITH NO SHORT TERM TREND
# OLD: node32.df <- node16.df %>% dplyr::filter(PercChange >= -70)
node32.df <- node16.df %>% dplyr::filter(PercChange >= -70 | is.na(PercChange))
# not a leaf, so no status yet
if(dim(node32.df)[1] == 0){
  node32.df$SynStatus <- character()
  node32.df$BinLabel <- character()
  node32.df$BinPath <- character()
}


node33.df <- node16.df %>% dplyr::filter(PercChange < -70)
if(dim(node33.df)[1] > 0){
  node33.df$SynStatus <- "Red"
  node33.df$BinLabel <- "Node33"
  node33.df$BinPath <- "Data Type is RelAbd OR AbsLBM >= 1.5, then Data Type is RelAbd OR AbsUBM >= 1, then Don't have RelLBM, then LongTrend >= 79, then PercChange < -70"
  #write.csv(node33.df,"test_node33.csv")
}


node64.df <- node32.df %>% dplyr::filter(LongTrend >= 233)
if(dim(node64.df)[1] > 0){
  node64.df$SynStatus <- "Green"
  node64.df$BinLabel <- "Node64"
  node64.df$BinPath <- " Data Type is RelIdx OR AbsLBM >= 1.5, then Data Type is RelIdx OR AbsUBM >= 1, then Don't have RelLBM, then LongTrend >= 79, then PercChange >= -70, then LongTrend >= 233"

}


node65.df <- node32.df %>% dplyr::filter(LongTrend < 233)
if(dim(node65.df)[1] > 0){
  node65.df$SynStatus <- "Amber"
  node65.df$BinLabel <- "Node65"
  node65.df$BinPath <- " Data Type is RelIdx OR AbsLBM >= 1.5, then Data Type is RelIdx OR AbsUBM >= 1, then Don't have RelLBM, then LongTrend >= 79, then PercChange >= -70, then LongTrend < 233"

}


data.out <- rbind(node2.na,node16.na,#node32.na,# REMOVED MAR 2024
                  node3.df,node17.df,node19.df,
                  node20.df,node21.df,node22.df,node23.df,
                  node33.df,
                  node36.df,node37.df,
                  node64.df,node65.df)


} # end StateOfTheSalmon3











#--------------------------------------------------------------------------------------------------------------------------------

# create summaries

summary.tab <- as.data.frame(matrix(NA,ncol=length(status.labels),dimnames=list("Tmp", status.labels)))
summary.tab <- bind_rows(summary.tab, as.data.frame(t(as.matrix(table(data.out$SynStatus)))))
summary.tab <- summary.tab[-1,,drop = FALSE]
dimnames(summary.tab)[[1]][1] <- "SynStatus"

#print("flag:summary.tab done")


if("IntStatus" %in% names(data.df)){

  # IF CategoricalRealist -> convert Int Status

   if(algorithm == "CategoricalRealist"){
        data.out$IntStatus <- dplyr::recode(data.out$IntStatus, RedAmber = "Red", AmberGreen = "Amber")
     }

  intstatus.summary  <- as.data.frame(t(as.matrix(table(data.out$IntStatus)))   )

  summary.tab <- bind_rows(summary.tab, intstatus.summary)
  dimnames(summary.tab)[[1]][2] <- "IntStatus"

  confusion.tab <- as.data.frame.matrix(table(data.out$SynStatus,data.out$IntStatus)) %>% rownames_to_column(var="Predicted")
  dimnames(confusion.tab)[[2]][-1] <-  paste0("Int",dimnames(confusion.tab)[[2]][-1])
  
  confusion.raw.tab <- as.data.frame.matrix(table(data.out$SynStatus,data.out$IntStatusRaw)) %>% rownames_to_column(var="Predicted")
  dimnames(confusion.raw.tab)[[2]][-1] <-  paste0("Int",dimnames(confusion.raw.tab)[[2]][-1])
  
  
}







# include status scores and error scores
#print("flag:error scores start")
if("IntStatus" %in% names(data.df)){ data.out <- left_join(data.out,scores.df %>% rename(IntStatus = Status,IntScore = Score))}
if(!("IntStatus" %in% names(data.df))){ data.out$IntStatus <- NA; data.out$IntScore <- NA}
#print("flag:error scores done")

data.out <- left_join(data.out,scores.df %>% rename(SynStatus = Status,SynScore = Score))

data.out$ErrorScore <- data.out$IntScore - data.out$SynScore




data.out$ErrorType <- "NA"
data.out[data.out$ErrorScore == 0 &!is.na(data.out$ErrorScore) , "ErrorType"] <- "None"
data.out[data.out$ErrorScore < 0 &!is.na(data.out$ErrorScore) , "ErrorType"] <- "PredWorse"
data.out[data.out$ErrorScore > 0 &!is.na(data.out$ErrorScore) , "ErrorType"] <- "PredBetter"


# ----------------------------------------------------------------------------------------------
# summarize accuracy, error types etc


# Freq of error scores


error.score.freq <- as.data.frame(as.matrix(table(data.out$ErrorScore))) %>% rename(All=V1) %>% rownames_to_column(var="ErrorScore")

if(!is.null(group.var)){

  error.score.freq.grp <- data.out %>% group_by_at(c(group.var,"ErrorScore")) %>%
    summarise(Freq = n()) %>%
    pivot_wider(id_cols = ErrorScore,names_from = all_of(group.var), values_from = Freq) %>%
    mutate(ErrorScore = as.character(ErrorScore))
  error.score.freq <- left_join(error.score.freq,error.score.freq.grp,by="ErrorScore")

}

error.score.freq <- bind_rows(error.score.freq,
                             cbind(ErrorScore = "Total",
                                   as.data.frame(t(colSums(error.score.freq %>%
                                                            select(-ErrorScore),na.rm=TRUE ))))
)



# Frequ of error types
error.type.freq <- as.data.frame(as.matrix(table(data.out$ErrorType))) %>% rename(All=V1) %>% rownames_to_column(var="ErrorType")

if(!is.null(group.var)){

error.type.freq.grp <- data.out %>% group_by_at(c(group.var,"ErrorType")) %>%
                                               summarise(Freq = n()) %>%
                                               pivot_wider(id_cols = ErrorType,names_from = all_of(group.var), values_from = Freq)
error.type.freq <- left_join(error.type.freq,error.type.freq.grp,by="ErrorType")

}

# fill in any missing categories
cat.list <- c("NA","None","PredWorse","PredBetter")
missing.cat.idx <-  !(cat.list  %in% error.type.freq$ErrorType)

# clean up this code!!!!!!!!!
if(sum(missing.cat.idx)>0){
  n.rows <- sum(missing.cat.idx)
  #n.cols <- length(names(error.type.freq))-1
  add.rows.df <-  error.type.freq[1:n.rows,]
  add.rows.df[,] <- 0
  add.rows.df[,1] <- cat.list[missing.cat.idx]

  error.type.freq <- bind_rows(error.type.freq, add.rows.df)


}





error.type.freq <- bind_rows(error.type.freq,
                             cbind(ErrorType = "Total(excl.NA)",
                                   as.data.frame(t(colSums(error.type.freq %>%
                                                             dplyr::filter(ErrorType != "NA") %>%
                                                              select(-ErrorType),na.rm=TRUE ))))
                                                          )


error.type.freq <- bind_rows(error.type.freq,
                             cbind(ErrorType = "PercCorrect",
                                  round( (error.type.freq %>% dplyr::filter(ErrorType == "None") %>% select(-ErrorType)) /
                                     (error.type.freq %>% dplyr::filter(ErrorType == "Total(excl.NA)") %>% select(-ErrorType))*100)
                             ),
                             cbind(ErrorType = "PercWorse",
                                   round( (error.type.freq %>% dplyr::filter(ErrorType == "PredWorse") %>% select(-ErrorType)) /
                                            (error.type.freq %>% dplyr::filter(ErrorType == "Total(excl.NA)") %>% select(-ErrorType))*100)
                             ),
                             cbind(ErrorType = "PercBetter",
                                   round( (error.type.freq %>% dplyr::filter(ErrorType == "PredBetter") %>% select(-ErrorType)) /
                                            (error.type.freq %>% dplyr::filter(ErrorType == "Total(excl.NA)") %>% select(-ErrorType))*100)
                             )
                             )






if("IntStatus" %in% names(data.df)){  # do Chi Sq tests

# NOTE: getting warning due to small sample size (for some bins),
# so using "simulate.p.value = TRUE"
# as per https://stats.stackexchange.com/questions/81483/warning-in-r-chi-squared-approximation-may-be-incorrect

  data.use <- data.out

  data.use$IntStatus <- dplyr::recode(data.out$IntStatus, DD = "None", UD = "None")

if(!("RedAmber" %in% rules.df$Status | "AmberGreen" %in% rules.df$Status)){
          data.use$IntStatus <- dplyr::recode(data.use$IntStatus, RedAmber = "Red", AmberGreen = "Amber")}

chisq.table <-   table(data.use$SynStatus,data.use$IntStatus)
chisq.test.out <- chisq.test(chisq.table,simulate.p.value = TRUE,B=2000)



chisq.list <- list(input = chisq.table,
                   statistic = chisq.test.out$statistic,
                   p.value = chisq.test.out$p.value,
				   p.label = p.label(chisq.test.out$p.value)
                   #observed = chisq.test.out$observed#,
                   #expected = chisq.test.out$expected
                   )


}




if(!("IntStatus" %in% names(data.df))){
accuracy.list <- list(error.scores.freq = error.score.freq,
                      error.type.freq = error.type.freq)
					  }
if("IntStatus" %in% names(data.df)){

accuracy.list <- list(error.scores.freq = error.score.freq,
                      error.type.freq = error.type.freq,
					  chisq = chisq.list)
}




out.obj <- list(rules = rules.df,
                data = data.out,
                summary = summary.tab %>% rownames_to_column(var="Type")  )


if("IntStatus" %in% names(data.df)){  # do confusion matrix
   out.obj <- c(out.obj, list(confusion = confusion.tab,confusion.raw = confusion.raw.tab, accuracy = accuracy.list))



   }






return(out.obj)

}



p.label <- function(x){

if(x >= 0.1){p.out <- ">0.1"}
if(x < 0.1){p.out <- "<0.1"}
if(x < 0.05){p.out <- "<0.05"}
if(x < 0.01){p.out <- "<0.01"}
if(x < 0.005){p.out <- "<0.005"}


return(p.out)

}



####################
# IDENTICAL REPLICATE WITH NEW NAME

#renaming the function
rapid_status <- synoptic