#' Measures of model fit for to assess ARIMA models
#'
#' @param arimaEstimation An object of class arima.
#' @param decim Specifies the number of decimals to export results. The default is 4.
#' @returns A dataframe with Log likelihood, Akaike information criterion, Bayesian information criterion, Invertible AR roots, Invertible MA roots

#' @examples
#' library(quantmod)
#' library(urca)
#' getSymbols("AAPL", src="yahoo")
#' type=unitRootDF_ABsequential(AAPL$AAPL.Close)
#' aapl_stationary=stationarize(ts(AAPL$AAPL.Close), type=type)
#' arimaEstimation=arima(aapl_stationary, order = c(1,0,0))
#' arimaModelFit(arimaEstimation, decim=2)

arimaModelFit=function(arimaEstimation, decim=4)
{
  modelOrder=eval(arimaEstimation$call$order)
  if(modelOrder[2]==0)
  {
    model=paste0("ARMA (", modelOrder[1], ",", modelOrder[3], ")")
  } else {
      model=paste0("ARIMA (", modelOrder[1], "," , modelOrder[2], ",", modelOrder[3], ")")
  }

  logLike=round(arimaEstimation$log, decim)
  AIC=round(arimaEstimation$aic, decim)

  #computing BIC

  nPar=length(arimaEstimation$coef)+1
  nObs=length(arimaEstimation$residuals)
  BIC=round(-2*logLike + log(nObs) * nPar, decim)

  #computing the AR and MA invertible roots

  ##extracting the AR coefficients
  arCoefficients=arimaEstimation$coef[1:arimaEstimation$arma[1]]
  maCoefficients=arimaEstimation$coef[(arimaEstimation$arma[1]+1):(arimaEstimation$arma[1]+arimaEstimation$arma[2])]
  ##computing the roots
  arRoots=polyroot(c(1, -arCoefficients))
  maRoots=polyroot(c(1, -maCoefficients))
  ##inverting the roots
  invertibleARroots=1/arRoots
  invertibleMAroots=1/maRoots
  invertibleARroots=paste(invertibleARroots, collapse = ", ")
  invertibleMAroots=paste(invertibleMAroots, collapse = ", ")

  #constructing the modelFit matrix
  modelFit=rbind(logLike, AIC, BIC, invertibleARroots, invertibleMAroots)
  rownames(modelFit)=c("Log likelihood", "Akaike information criterion", "Bayesian information criterion",
                       "Invertible AR roots", "Invertible MA roots")
  colnames(modelFit)=model
  modelFit
}
