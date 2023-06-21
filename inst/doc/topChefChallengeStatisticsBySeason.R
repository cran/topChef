## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----ChallengeStatisticsVisualization, eval=TRUE,echo=FALSE,message=FALSE-----
library(topChef)
library(tidyr)
library(dplyr)
library(ggplot2)

    chefdetails <- topChef::chefdetails %>% filter(seasonNumber == 20)
    challengewins <- topChef::challengewins %>% filter(seasonNumber == 20)
    
    ###########################################################################
    ## Set up data
    ###########################################################################
    currentstats <- challengewins %>%
      select(!rating) %>% 
      full_join(chefdetails %>% 
                  select(series,season,seasonNumber,chef,placement,gender)) 
    
      ## number of each type (combination of challenge + outcome)
        nums <- currentstats %>%
          group_by(chef,challengeType,outcome,placement,gender) %>%
          summarise(n=n()) %>%
          # remove groups we don't want to visualize
          filter(!(is.na(outcome)))
        
        nums$outcome <- factor(nums$outcome,levels=c("OUT","LOW","IN"
                                                     ,"HIGH","WIN"))
      
        ## Create bar charts for #s of each combo of challenge type-outcome
          nums %>%
            filter(!(outcome %in% c("IN","OUT"))) %>%
            ggplot(aes(x=n,y=chef,color=outcome,fill=outcome)) +
            geom_bar(stat="identity" ,alpha=.8 ) +
            facet_wrap(challengeType ~ outcome) +
            scale_fill_manual(values=c("#ffbc69","#a3cce9","#1170AA"))+
            scale_color_manual(values=c("#ffbc69","#a3cce9","#1170AA"))+
            theme_minimal() +
            theme(legend.position="none") +
            labs(title="Challenge summary for Season 20")
    


## ----ChallengeStatisticsVisualization_code, eval=FALSE,echo=TRUE,message=FALSE----
#  library(topChef)
#  library(tidyr)
#  library(dplyr)
#  library(ggplot2)
#  
#      chefdetails <- topChef::chefdetails %>% filter(seasonNumber == 20)
#      challengewins <- topChef::challengewins %>% filter(seasonNumber == 20)
#  
#      ###########################################################################
#      ## Set up data
#      ###########################################################################
#      currentstats <- challengewins %>%
#        select(!rating) %>%
#        full_join(chefdetails %>%
#                    select(series,season,seasonNumber,chef,placement,gender))
#  
#        ## number of each type (combination of challenge + outcome)
#          nums <- currentstats %>%
#            group_by(chef,challengeType,outcome,placement,gender) %>%
#            summarise(n=n()) %>%
#            # remove groups we don't want to visualize
#            filter(!(is.na(outcome)))
#  
#          nums$outcome <- factor(nums$outcome,levels=c("OUT","LOW","IN"
#                                                       ,"HIGH","WIN"))
#  
#          ## Create bar charts for #s of each combo of challenge type-outcome
#            nums %>%
#              filter(!(outcome %in% c("IN","OUT"))) %>%
#              ggplot(aes(x=n,y=chef,color=outcome,fill=outcome)) +
#              geom_bar(stat="identity" ,alpha=.8 ) +
#              facet_wrap(challengeType ~ outcome) +
#              scale_fill_manual(values=c("#ffbc69","#a3cce9","#1170AA"))+
#              scale_color_manual(values=c("#ffbc69","#a3cce9","#1170AA"))+
#              theme_minimal() +
#              theme(legend.position="none") +
#              labs(title="Challenge summary for Season 20")
#  

