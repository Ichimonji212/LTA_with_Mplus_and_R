
## Latent Variable Modeling using Mplus with R##
## Created by Ryan Ji    ##
## updated on Feb 9 2019 ##

## Section 1 {MplusAutomation}
#Hallquist, M. N. & Wiley, J. F. (2018). MplusAutomation: An R Package for 
#Facilitating Large-Scale Latent Variable Analyses in Mplus. Structural Equation Modeling, 
#25, 621-638. doi: 10.1080/10705511.2017.1402334.

## R syntax for using R {MplusAutomation} to run Mplus Latent Variable Modeling ## 
#install.packages("MplusAutomation", .Library)
library(MplusAutomation)
sessionInfo()

## House Keeping Work ###
main_path<-"/Users/ryanji/Documents/Research/Jis_project/lta_to_run"
## set up the wd
setwd(main_path)
getwd()


## Latent Classs Modeling ##

## set up the sub-path for each folder for lCA
LCA_path<-paste(main_path,'/LCA', sep="")
  y2path<-paste(LCA_path,'/Y2_LCA',sep ="") 
  y3path<-paste(LCA_path,'/Y3_LCA',sep ="")


## run models from 2-class to 4-class on yr2 and yr3
  runModels(LCA_path, recursive=TRUE) ## only to run LCA models

## read the model output from mplus gh5 files
## which are saved in Y2_LCA and Y3_LCA folders
LCA2 <- readModels(y2path,what="summaries")
LCA3 <- readModels(y3path,what="summaries")

allOutput<-append(LCA2,LCA3) ## combine the outputs from Y2 and Y3

SummaryTable(allOutput ,keepCols=c("Title", "LL", "AIC", "BIC", 
                                   "BLRT_PValue","T11_LMR_PValue"), 
             sortBy="Title",display=TRUE)

# Conclusion:  3-Class solutions on both times are the selected models

## output the summary to the HTML file
#HTMLSummaryTable(allOutput ,keepCols=c("Title", "LL", "AIC", "BIC", 
#                                       "BLRT_PValue","T11_LMR_PValue"), 
#                 sortBy="Title",display=TRUE)

## Latent Transition Modeling ##
## set up lta subpath the freely estimated LTA and measurement invariance LTA 
## saved in this folder

LTA_path<-paste(main_path,'/LTA', sep="")
  # run the two models
  runModels(LTA_path, recursive=TRUE) ## only to run LCA models
  
    #read the output
    lta_output<-readModels(LTA_path, what="summaries")
  
    # Model Comparison nested vs complete
    compareModels(lta_output[["y2_3_lta_mi.out"]], lta_output[["y2_3_lta_free.out"]], 
                show=c("summaries"), diffTest=T, showNS=F)

## Conclusion: the measurement invariance across yr2 and yr3 holds
    
####################################################################################    
## Section 2 Data Viz using Mplus gh5 for Mac Users    
    ##### plot 3-latent class solution in
    #load Mplus R source code
  
    
    source("mplus.R")  # mplus.R should be saved in the folder 
    dir()
    
    # check with mplus output conditional item response probablities
    # year2 solution
    mplus.get.estimated_probabilities('./LCA/Y2_LCA/y2_3_lca.gh5',cat1=2) 
    
    # year 3 solution
    mplus.get.estimated_probabilities('./LCA/Y3_LCA/y3_3_lca.gh5',cat1=2) 
    
    #rough plot
    par(mfrow=c(1,2))
    mplus.plot.estimated_probabilities('./LCA/Y2_LCA/y2_3_lca.gh5', cat1=2)
    mplus.plot.estimated_probabilities('./LCA/Y3_LCA/y3_3_lca.gh5', cat1=2)
    
    ## reset 
    par(mfrow=c(1,1))
    #dev.off()
    
    ##data prep for gglot
    ## YEAR2 
    y2plot<-mplus.get.estimated_probabilities('./LCA/Y2_LCA/y2_3_lca.gh5',cat1=2) 
    y2plot_t<-t(y2plot)
    y2plot_dat<-data.frame(y2plot_t)
    y2plot_dat["CLASS"]<-c("CLS2","CLS1","CLS3") # to consistent with LTA output
    names(y2plot_dat)[1:7]<-lessR::to("Y2BC", 7)
    head(y2plot_dat)
    
    y2plot_dat$cls<-factor(y2plot_dat$cls) #the cls column is a factor
    # change wide to long format dataset
    y2plot_long <- tidyr::gather(y2plot_dat, items, prob, Y2BC1:Y2BC7, factor_key=TRUE)
    y2plot_long
    
    library(ggplot2)
    p2 <- ggplot(y2plot_long, aes(x=CLASS, y=prob, fill=items)) + 
      theme(axis.text=element_text(size=8)) +
      theme(legend.position = "left",legend.text = element_text(size=8),
            legend.title = element_text(size=8)) +
      theme(axis.title.x = element_text(size=8))+
      theme(axis.title.y = element_text(size=8))+
      geom_bar(stat="identity", position=position_dodge()) + 
      scale_fill_brewer(palette="Paired") +
      ggtitle("Year 2")
    p2
    
    ## YEAR3
    y3plot<-mplus.get.estimated_probabilities('./LCA/Y3_LCA/y3_3_lca.gh5',cat1=2)
    y3plot_t<-t(y3plot)
    y3plot_dat<-data.frame(y3plot_t)
    y3plot_dat["CLASS"]<-c("CLS2","CLS1","CLS3") #consistent with LTA output
    names(y3plot_dat)[1:7]<-lessR::to("Y3BC", 7)
    head(y3plot_dat)
    
    y3plot_dat$CLASS<-factor(y3plot_dat$CLASS) #the cls column is a factor
    y3plot_long <- tidyr::gather(y3plot_dat, items, prob, Y3BC1:Y3BC7, factor_key=TRUE)
    y3plot_long
    
    p3 <- ggplot(y3plot_long, aes(x=CLASS, y=prob, fill=items)) + 
      theme(axis.text=element_text(size=8)) +
      theme(legend.position = "left",legend.text = element_text(size=8),
            legend.title = element_text(size=8)) +
      theme(axis.title.x = element_text(size=8))+
      theme(axis.title.y = element_text(size=8))+
      geom_bar(stat="identity", position=position_dodge()) + 
      scale_fill_brewer(palette="Paired") +
      ggtitle("Year 3")
    p3
    
    ##combine multiple plots in one output
    library(ggpubr)
    ggarrange(p2, p3, ncol = 1, nrow = 2, common.legend = TRUE, legend="bottom")
    
    ########################Latent Transtion Model####################################
  
    # check with mplus output conditional item response probablities
    mplus.get.estimated_probabilities('./LTA/y2_3_lta_mi.gh5',cat1=2) # check with mplus output
    mplus.plot.estimated_probabilities('./LTA/y2_3_lta_mi.gh5', cat1=2)
  
    ##data prep for gglot for lta results
    ltaplot<-mplus.get.estimated_probabilities('./LTA/y2_3_lta_mi.gh5',cat1=2) # check with mplus output
    ltaplot_t<-t(ltaplot) #transpose the dataset
    ltaplot_dat<-data.frame(ltaplot_t)
    
    library(lessR)
    ltaplot_dat["CLASS"]<-c("CLS11","CLS12","CLS13","CLS21","CLS22",
                            "CLS23","CLS31","CLS32","CLS33") #insert latent variable
    names(ltaplot_dat)[1:7]<-lessR::to("Y2BC", 7)
    names(ltaplot_dat)[8:14]<-lessR::to("Y3BC", 7)
    
    ltaplot_dat
    
    ## subset data for plot
    y2items<-lessR::to("Y2BC", 7)
    ltaplot_dat1<-dplyr::filter(ltaplot_dat, 
                                CLASS %in% c("CLS11","CLS22","CLS33")) %>%
                                dplyr::select(y2items,CLASS)  
    
    ltaplot_dat1$CLASS<-factor(ltaplot_dat1$CLASS) #the cls column is a factor
    ltaplot_long <- tidyr::gather(ltaplot_dat1, items, prob, Y2BC1:Y2BC7, factor_key=TRUE)
    ltaplot_long

    
    
    ## useing ggplot to plot the MI results
    library(ggplot2)
    p23 <- ggplot(ltaplot_long, aes(x=CLASS, y=prob, fill=items)) + 
      theme(axis.text=element_text(size=8)) +
      theme(legend.position = "left",legend.text = element_text(size=8),
            legend.title = element_text(size=8)) +
      theme(axis.title.x = element_text(size=8))+
      theme(axis.title.y = element_text(size=8))+
      geom_bar(stat="identity", position=position_dodge()) + 
      scale_fill_brewer(palette="Paired") +
      ggtitle("LTA Year2 and Year3")
    p23
    
    ##combine multiple plots in one output
    library(ggpubr)
    ggarrange(p2, p3, p23,ncol = 3, nrow = 1, common.legend = TRUE, legend="bottom")
    
    #####################################################################################
    #Input LCA results in R for further analysis
    # setwd("/Users/ryanji/Documents/Research/Jis_project/")
    # dir()
    # library(foreign)
    # 
    # #import data
    # y2_3_lca<-read.csv("y2_3_lca.csv", header = F, sep='')
    # head(y2_3_lca)
    # 
    # y3_3_lca<-read.csv("y3_3_lca.csv", header = F, sep='')
    # head(y3_3_lca)
    # 
    # head(y2_3_lca)
    # names(dat3)
    # 
    # #data clone
    # dat_y2<-y2_3_lca 
    # dat_y3<-y3_3_lca 
    # 
    # # data preparation
    # #install.packages("lessR")
    # 
    # # assign the variable name in the dataset
    # y2items<-lessR::to("Y2BC", 7)
    # y2cprob<-lessR::to("Y2CPROB",3)
    # names(dat_y2)<-c(y2items,"OBS",y2cprob,"Y2CLS")
    # 
    # 
    # y3items<-lessR::to("Y3BC", 7)
    # y3cprob<-lessR::to("Y3CPROB",3)
    # names(dat_y3)<-c(y3items,"OBS",y3cprob,"Y3CLS")
    # head(dat_y3)
    # 
    # 
    # #merge dataset
    # 
    # dim(dat_y2) #492 used for analysis
    # dim(dat_y3) #493 used for analysis
    # 
    # #outer join
    # dat_y23_out<-merge(dat_y2,dat_y3, by ="OBS", all = T) 
    # dim(dat_y23_out) #the orginal sample size is 496
    # 
    # #inner join
    # dat_y23_in<-merge(dat_y2,dat_y3, by ="OBS") 
    # dim(dat_y23_in)  #the final output includes 489 obs
    # 
    # attach(dat_y23_in)
    # table(Y2CLS, Y3CLS)
    # detach(dat_y23_in)
    
    
    
    
    
    
    
    
    
    
    



