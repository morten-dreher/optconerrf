#' Calculate Expected Second-stage Information
#' @name getMinMaxInformation
#'
#' @description Calculate the minimum and maximum information
#'
#'
#'

#Achtung Formel nochmal checken!
getMinMaxInformation<-function(design, delta_min=0.001, delta_max=NULL){
  minInfo_alpha=NULL
  maxInfo_alpha=NULL
  maxInfo_cond_error=NULL
  minInfo_cond_error=NULL

  if(design$alpha1!=0){
    epsilon<-0.0001
    estimated_delta<-qnorm(1-(design$alpha1+epsilon))*design$firstStageInformation^(-1/2)
    delta<-max(delta_min, estimated_delta)
    minInfo_alpha<-(qnorm(design$conditionalPower)+qnorm(1-(design$alpha1+epsilon)))^2/delta
    print(paste0("minInfo_alpha", minInfo_alpha))
  }
  if(design$alpha0!=1){
    estimated_delta<-qnorm(1-design$alpha0)*design$firstStageInformation^(-1/2)
    delta<-min(delta_max, estimated_delta)
    maxInfo_alpha<-(qnorm(design$conditionalPower)+qnorm(1-design$alpha0))^2/delta
    print(paste0("maxInfo_alpha", maxInfo_alpha))
  }
  if(design$maximumConditionalError!=1&!is.null(delta_max)){
    minInfo_cond_error<-(qnorm(design$conditionalPower)+qnorm(1-design$maximumConditionalError))^2/delta_max
    print(paste0("minInfo_cond_error", minInfo_cond_error))
  }
  if(design$minimumConditionalError!=0){
    maxInfo_cond_error<-(qnorm(design$conditionalPower)+qnorm(1-design$minimumConditionalError))^2/delta_min
    print(paste0("maxInfo_cond_error", maxInfo_cond_error))
  }

  minInfo<-max(minInfo_alpha, minInfo_cond_error)
  maxInfo<-min(maxInfo_alpha, maxInfo_cond_error)

return(c("minInfo"=minInfo, "maxInfo"=maxInfo))
}

#test1<-getDesignOptimalConditionalErrorFunction(alpha=0.05, alpha1 = 0.005, alpha0 = 0.5, conditionalPower = 0.8,
#                                                delta1=0.1, firstStageInformation = 1.2, likelihoodRatioDistribution = "maxlr")
#
#test2<-getDesignOptimalConditionalErrorFunction(alpha=0.05, alpha1 = 0, alpha0 = 1, conditionalPower = 0.8,
#                                                delta1=0.1, firstStageInformation = 1.2, likelihoodRatioDistribution = "maxlr",
#                                                minimumConditionalError= 0.01, maximumConditionalError = 0.5)
#
#getMinMaxInformation(test2, delta_max = 1, delta_min = 0.1)

