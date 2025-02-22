## CALCULATE THE ENTROPY OF THE MOVEMENT SEQUENCE GENERATED BY THE FORAGER
## lower entropy indicates lower uncertainty of the next symbol in the sequence
## with the previous symbols better predicting the following symbols, which 
## can identify traplines where subsequences get repeated and lower the entropy

# load compiler to speed up computation
library(compiler)

entropy <- function(SEQ,max3=110,max4=30,max5=10) {

  # this function computes for any sequence SEQ, its Shannon entropy, and the 
  # corrected conditional entropies at orders 1 to 5 following Porta et al. (1998)
  # the output is: 
    # O_corr: the vector of the Shannon entropy followed by the corrected 
    # conditional entropies for order 1 to 5
  
  # max4 and max5 are the maximum numbers of different sites visited in SEQ for 
  # the conditional entropies of order 4 and 5 to be computed, respectively.
  # memory size problems can arise when computing the highest-order conditional 
  # transition matrices, for sequences with a high number of different sites used.
    # in this case, an error message is given, you can try to increase the 
    # amount of memory used by R using memory.limit()
    # otherwise, max4 and max5 should be set to smaller values
  # if the number of different sites visited is superior to max3, the computation 
  # is stopped and returns a series of NA
  
  # In any case, the relevant estimation of the minimum conditional entropy is 
  # warranted only if a minimum is reached in O_corr
  # that is, that the conditional entropy does not significantly decrease until 
  # the last value (that is, O5, or O4 or O3 if a very high number of different 
  # sites are visited)
  
  # find the unique coconuts visited in the sequence, use that number as the 
  # number of possible transition sites (base)
  sites <- unique(SEQ)
  base <- length(sites)

  ##----------------------------------------------------------------------------
  ##   order 0: compute standard conditional entropy
  ##----------------------------------------------------------------------------
  
  # find the baseline proportion of times visited for each coconut/site over the
  # total number of visits to all coconuts/sites
  prop <- sapply(sites,function(x) length(SEQ[SEQ==x])/length(SEQ))

  # compute the 0th order or standard entropy by summing together the proportion
  # of times that coconut/site was visited times the log base sites of that same 
  # prop, negate to make positive
  O0 <- -sum(sapply(1:base,function(x) prop[x]*log(prop[x],base)))

  ##----------------------------------------------------------------------------
  ##   order 1: compute conditional entropy based on transitions
  ##----------------------------------------------------------------------------

  # create and fill matrix of coconut to coconut transitions for all potential 
  # transitions
  p1 <- matrix(nrow = base, ncol = base, 0)
  
  # count the number of transitions between one coconut to another
  for (t in 1:(length(SEQ) - 1)) {
    p1[which(sites == SEQ[t]), which(sites == SEQ[t + 1])] <- 
      p1[which(sites == SEQ[t]), which(sites == SEQ[t + 1])] + 1
  }
  
  # create and fill matrix of conditional probabilities of each transition 
  prop1 <- matrix(nrow = base, ncol = base, 0)
  
  # loop through sequence and matrix and compute baseline probabilities of 
  # transitions that occurred within the sequence
  for (i in 1:base) {
    # if this transition occurred, calculate as proportion of all transitions 
    if(sum(p1[i, ]) > 0) {
      prop1[i, ] <- p1[i, ] / sum(p1[i, ])
    }
  }

  O1 <- 0
  
  # compute conditional probabilities using baseline probabilities 
  for(i in 1:base)
  {
    for(j in 1:base)
    {
      # if transition occurred, calculate its conditional probability 
      if(prop1[i,j] > 0) { 
        # "sum" together the conditional probabilities for each of the observed 
        # transitions
        O1 <- O1 - prop[i] * prop1[i,j] * log(prop1[i,j], base)
      }
    }
  }
  
  # assign O0 probabilities to values of single transitions that occurred once,
  # this serves as a noise term that corrects for bias towards zero with long
  # sequences with many different transitions represented 
  O1_corr <- O1 + (length(which(p1==1))/(length(SEQ)-1))*O0

  # clear matrix
  rm(prop1)

  ##----------------------------------------------------------------------------
  ##   order 2: compute conditional entropy based on two previous transitions
  ##----------------------------------------------------------------------------

  # create and fill matrix counting number of three length subsequences that 
  # occurred within the sequence
  p2 <- matrix(nrow = base * base, ncol = base, 0) # exponentially more to consider
  
  # loop through sequence and fill matrix
  for(t in 1:(length(SEQ) - 2)) {
    # establish new row num as key to defining specific 3L subsequences
    row_num <- (which(sites == SEQ[t]) - 1) * length(sites) + which(sites == SEQ[t+1])
    
    # now use that to count how many times those subsequences occurred
    p2[row_num, which(sites==SEQ[t+2])] <- 
      p2[row_num, which(sites==SEQ[t+2])] + 1
  }
  
  # create and fill matrix of baseline proportions of 3L subsequence occurances
  prop2 <- matrix(nrow = base*base, ncol = base, 0)
  
  # loop through rows and calculate proportions
  for(i in 1:nrow(p2)) {
    # if the row is greater than 0, then compute the baseline probablity of 
    # that set of transitions (that row)
    if(sum(p2[i, ]) > 0) {
      prop2[i, ] <- p2[i, ] / sum(p2[i, ])
    }
  }

  # compute global proportion
  p1_glob <- p1 / sum(p1)
  
  # remove p1 matrix
  rm(p1)
  
  O2 <- 0
  
  # compute conditional probability from baseline probabilites
  for(i in 1:base) {
    for(j in 1:base) {
      # use rownum for index based on 2L subsequences
      row_num <- (i - 1) * base + j
      for(k in 1:base) {
        # only compute for 3L subsequences that occurred 
        if(prop2[row_num,k] > 0) { 
          # sum together the conditional probabilities multiplied by the 
          # global probabilities of the 2L transitions
          O2 <- O2 - p1_glob[i,j] * prop2[row_num,k] * log(prop2[row_num,k], base)
        }
      }
    }
  }
  
  # correct value with noise 
  O2_corr <- O2 + (length(which(p2==1)) / (length(SEQ)-2)) * O0
  
  # remove unnecessary elements
  rm(prop2)
  rm(p1_glob)

  ##----------------------------------------------------------------------------
  ##   order 3: compute conditional entropy based on three previous transitions
  ##----------------------------------------------------------------------------

  # check to ensure it is below processing threshhold 
  if(base <= max3) {
    # create matrix to fill, exponentially larger again
    p3 <- matrix(nrow = base^3, ncol = base, 0)
    
    for(t in 1:(length(SEQ)-3)) {
      # find unique positions within matrix for each 4L transition
      row_num <- (which(sites == SEQ[t]) - 1) * (base^2) + 
        (which(sites == SEQ[t+1]) - 1) * base + which(sites == SEQ[t+2])
      
      # count the number of transitions that occurred 
      p3[row_num, which(sites == SEQ[t+3])] <- 
        p3[row_num, which(sites == SEQ[t+3])] + 1
    }
  
    # create and fill matrix for baseline proportions
    prop3 <- matrix(nrow=base^3,ncol=base,0)
    
    # create baseline prob for any 4L subsequences that occurred 
    for(i in 1:nrow(p3)) {
      if(sum(p3[i, ]) > 0) { 
        prop3[i, ] <- p3[i, ] / sum(p3[i, ])
      }
    }
    
    # create global proportion matrix for 3L transitions, then remove old one
    p2_glob <- p2 / sum(p2)
    rm(p2)

    # calculate conditional prob using baseline prob 
    O3 <- 0
    for(i in 1:base) {
      for(j in 1:base) {
        # establish row index based on o1 transitions
        row_num_2 <- (i - 1) * base + j
        for(k in 1:base) {
          # repeat for o2 transitions 
          row_num <- (i - 1) * (base^2) + (j - 1) * base + k
          for(l in 1:base) {
            # calculate cond prob from proper row indices
            if(prop3[row_num,l]>0) { 
              O3 <- O3 - p2_glob[row_num_2,k] * prop3[row_num,l] * 
                log(prop3[row_num,l],base)
            }
          }
        }
      }
    }
    
    # correct entropy value with noise 
    O3_corr <- O3 + (length(which(p3==1)) / (length(SEQ) - 3)) * O0
    
    # clear matrices
    rm(prop3)
    rm(p2_glob)

  ##----------------------------------------------------------------------------
  ##   order 4: compute conditional entropy for previous four transitions
  ##----------------------------------------------------------------------------

    if(base <=max4) {
      p4 <- matrix(nrow=base^4,ncol=base,0)
      for(t in 1:(length(SEQ)-4)) {
        row_num <- (which(sites==SEQ[t])-1)*(base^3) + 
          (which(sites==SEQ[t+1])-1)*(base^2) + (which(sites==SEQ[t+2])-1)*base +
          which(sites==SEQ[t+3])
        p4[row_num, which(sites==SEQ[t+4])] <- 
          p4[row_num, which(sites==SEQ[t+4])]+1
      }
    
      prop4 <- matrix(nrow=base^4,ncol=base,0)
      for(i in 1:nrow(p4)) {
        if(sum(p4[i, ])>0) { 
          prop4[i, ] <- p4[i, ] / sum(p4[i, ])
        }
      }

      p3_glob <- p3/sum(p3)
      rm(p3)

      O4 <- 0
      for(i in 1:base) {
        for(j in 1:base) { 
          for(k in 1:base) {
            row_num_3 <- (i - 1) * (base^2) + (j - 1) * base + k
            for(l in 1:base) {
              row_num <- (i - 1) * (base^3) + (j - 1) * (base^2) + (k - 1) * base + l
              for(m in 1:base) {
                if(prop4[row_num,m] > 0) { 
                  O4 <- O4 - p3_glob[row_num_3,l] * prop4[row_num,m] * 
                  log(prop4[row_num,m],base)
                }
              }
            }
          }
        }
      }
      
      O4_corr <- O4 + (length(which(p4==1))/(length(SEQ)-4))*O0
      rm(prop4)
      rm(p3_glob)


  ##----------------------------------------------------------------------------
  ##   order 5: compute conditional entropy based on five previous transitions
  ##----------------------------------------------------------------------------

      if(base <= max5) {
        p5 <- matrix(nrow=base^5,ncol=base,0)
        for(t in 1:(length(SEQ)-5)) {
          row_num <- (which(sites==SEQ[t]) - 1) * (base^4) + 
            (which(sites==SEQ[t+1]) - 1) * (base^3) + 
            (which(sites==SEQ[t+2]) - 1) * (base^2) + 
            (which(sites==SEQ[t+3]) - 1) * base +
            which(sites==SEQ[t+4])
          
          p5[row_num, which(sites==SEQ[t+5])] <- 
            p5[row_num, which(sites==SEQ[t+5])] + 1
        }
    
        prop5 <- matrix(nrow = base^5, ncol = base, 0)
        for(i in 1:nrow(p5)) {
          if(sum(p5[i, ]) > 0) { 
            prop5[i,] <- p5[i,]/sum(p5[i,])
          }
        }

        p4_glob <- p4 / sum(p4)
        rm(p4)

        O5 <- 0
        for(i in 1:base) {
          for(j in 1:base) {
            for(k in 1:base) {
              for(l in 1:base) {
                row_num_4 <- (i-1)*(base^3) + (j-1)*(base^2) + (k-1)*base + l
                for(m in 1:base) {
                  row_num <- (i-1)*(base^4) + (j-1)*(base^3) + (k-1)*(base^2) + (l-1)*base + m
                  for(n in 1:base) {
                    if(prop5[row_num,n]>0) {
                      O5 <- O5 - p4_glob[row_num_4,m] * prop5[row_num,n] * 
                        log(prop5[row_num,n],base)
                    }
                  }
                }
              }
            }
          }
        }
        
        O5_corr <- O5 + (length(which(p5==1))/(length(SEQ)-5))*O0
        
        # if made it this far, return all corrected O values (plus 0)
        O_corr <- c(O0,O1_corr,O2_corr,O3_corr,O4_corr,O5_corr)
      }
      else {
        # return just the four (plus 0)
        O_corr <- c(O0,O1_corr,O2_corr,O3_corr,O4_corr)
      }
    }
    else {
      # return just the three (plus 0)
      O_corr <- c(O0,O1_corr,O2_corr,O3_corr)
    }
  }
  else {
    # return nothing
    O_corr <- rep(NA, times = 6)
  }
  
  return(O_corr)

}
