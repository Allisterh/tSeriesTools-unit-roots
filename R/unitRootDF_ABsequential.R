#' Dickey-Fuller test applied following the Ayat and Burridge (2000) sequential procedure
#'
#' @param x An object of class ts.
#' @param pvalue Specifies the threshold for rejecting the null hypothesis. The default is 0.05.
#' @param ic Lag selection based on "AIC" or "BIC". The default is "BIC".
#' @returns A text: "Stationary, no trend", "Stationary around a linear trend", "Random walk with drift", "Unit root and linear trend".
#' @references
#' Ayat, L., Burridge, P. (2000), Unit root tests in the presence of uncertainty about the non-stochastic trend, Journal of Econometrics, Vol. 95, Issue 1, pp. 71-96, DOI: 10.1016/S0304-4076(99)00030-5.
#' https://cran.r-project.org/web/packages/urca/index.html
#' @examples
#' library(quantmod)
#' library(urca)
#' getSymbols("AAPL", src="yahoo")
#' unitRootDF_ABsequential(AAPL$AAPL.Close)
#' unitRootDF_ABsequential(AAPL$AAPL.Close, 0.01)

unitRootDF_ABsequential=function(x, pvalue=0.05, ic="BIC")
{
  ADF1=ur.df(x, type = "trend", selectlags = ic)
  ADF1_stat=attributes(ADF1)$teststat[1] #the statistic of the test
  criticalValue_1=attributes(ADF1)$cval[1,1] #critical value at 1%
  criticalValue_5=attributes(ADF1)$cval[1,2] #critical value at 5%
  criticalValue_10=attributes(ADF1)$cval[1,3] #critical value at 10%

  if(pvalue==0.01)
  {
    criticalValue=criticalValue_1
  } else {
    if(pvalue==0.05)
    {
      criticalValue=criticalValue_5
    } else {
      criticalValue=criticalValue_10
    }
  }

  if(ADF1_stat>criticalValue)
  {
    diff_x=diff(x)
    trend=1:length(diff_x)
    regression=lm(diff_x~trend, data.frame(trend, diff_x))
    results=summary(regression)
    pvalueTrend=results$coefficients[2,4] #p-value of trend's coefficient

    if(pvalueTrend>pvalue)
    {
      ADF2=ur.df(x, type = "drift", selectlags = "BIC")
      ADF2_stat=attributes(ADF2)$teststat[1] #the statistic of the test
      criticalValue_1=attributes(ADF2)$cval[1,1] #critical value at 1%
      criticalValue_5=attributes(ADF2)$cval[1,2] #critical value at 5%
      criticalValue_10=attributes(ADF2)$cval[1,3] #critical value at 10%

      if(pvalue==0.01)
      {
        criticalValue=criticalValue_1
      } else {
        if(pvalue==0.05)
        {
          criticalValue=criticalValue_5
        } else {
          criticalValue=criticalValue_10
        }
      }

      if(ADF2_stat>criticalValue)
      {
        type=c("Random walk with drift")
        type
      } else {
        type=c("Stationary, no trend")
        type
      }
    } else {
      type=c("Unit root and linear trend")
      type
    }
  } else {
    pvalueTrend=attributes(ADF1)$testreg$coefficients[3,4]
    if(pvalueTrend>pvalue)
    {
      type=c("Stationary, no trend")
      type
    } else {
      type=c("Stationary around a linear trend")
      type
    }
  }
}
