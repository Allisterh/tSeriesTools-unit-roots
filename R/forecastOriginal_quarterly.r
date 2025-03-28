#' Takes an ARIMA estimation on stationary and seasonally adjusted series and returns the forecast on the original, non-stationary, not seasonally adjusted series
#'
#' @param arimaEstimation An object of class arima.
#' @param type Specifies the type of the original time series.
#'   The options are: "Stationary around a linear trend", "Random walk with drift", "Unit root and linear trend"
#' @param seasonalCoeff A vector of seasonal coefficients.
#' @param seasonalityType "additive", "multiplicative", or "none".
#' @param nQuarters Number of quarters to forecast. Default is 4.
#' @param year The year when the forecast should start.
#' @param quarter The quarter when the forecast should start.
#' @param saSeries A vector with the seasonally adjusted series.
#' @returns The forecasted values for the original non-stationary, not seasonally adjusted series.
#' @examples
#' library(urca)
#' library (forecast)
#' library(wooldridge)
#' data('intqrt')
#' plot(intqrt$r3, type="l")
#' Tbill3M=ts(as.numeric(intqrt$r3), start=c(1960,1), frequency=4)
#' Tbill3M_dec=decompose(Tbill3M, type ="multiplicative")
#' seasonalCoeff=Tbill3M_dec$figure
#' Tbill3M_sa=ts(Tbill3M/Tbill3M_dec$seasonal, start=c(1960,1), frequency=4)
#' type=unitRootDF_ABsequential(Tbill3M_sa)
#' Tbill3M_stationary=stationarize(ts(Tbill3M_sa, start=c(1960,1), frequency=4), type=type)
#' arimaEstimation=arima(Tbill3M_stationary, order = c(1,0,0))
#' forecastOriginal_quarterly(arimaEstimation, type, seasonalCoeff, seasonalityType="multiplicative", nQuarters=8, year=1991, quarter=1, saSeries=Tbill3M_sa)

forecastOriginal_quarterly=function(arimaEstimation, type, seasonalCoeff, seasonalityType="none", nQuarters=4, year, quarter, saSeries, predictionIntervals=c(80,90))
{
  modelOrder=eval(arimaEstimation$call$order)
  if(modelOrder[2]==0)
  {
    model=paste0("ARMA (", modelOrder[1], ",", modelOrder[3], ")")
  } else {
    model=paste0("ARIMA (", modelOrder[1], "," , modelOrder[2], ",", modelOrder[3], ")")
  }

  #Step 1: Forecast the stationary series
	forecast=forecast(arimaEstimation, h=nQuarters, level=predictionIntervals) #the forecasted values with prediction intervals
	forecast_x_sa_stationary=as.data.frame(forecast)
	for(series in 1:(length(predictionIntervals)*2+1))
	{
		forecast_x_sa_stationary_ts=ts(forecast_x_sa_stationary[,series], start=c(year,quarter), frequency=4)
		#Step 2: Forecast the original series
		if(type=="Stationary, no trend")
		{
			#in this case, the seasonally adjusted forecasted series is the same as the seasonally adjusted stationary series:
			forecast_x_sa=forecast_x_sa_stationary_ts
		} else {
			if(type=="Stationary around a linear trend")
			{
				trend=1:length(x)
				regression=lm(x~trend, data.frame(trend, x))
				x_stationary=regression$residuals
				#x_stationary=ts(x_stationary, start=c(yearStartSeries, quarterStartSeries), frequency=4)
				intercept=summary(regression)$coeff[1,1]
				betaTrend=summary(regression)$coeff[2,1]

				trendF=(arimaEstimation$nobs+1):(arimaEstimation$nobs+nQuarters)
				forecast_x_sa=intercept + betaTrend*trendF + forecast_x_sa_stationary_ts
				forecast_x_sa=ts(forecast_x_sa, start=c(year,quarter), frequency=4)
			} else {
				if(type=="Random walk with drift")
				{
					###since the first difference is the stationary series, we compute the original sa series by adding the forecasted difference to the
					####last element of the series, and so on....
					yearStartSeries=year-as.integer(length(saSeries)/4)
					q=(length(saSeries)/4-as.integer(length(saSeries)/4))/0.25
					quarterStartSeries=quarter-q
					x_sa_ts=ts(saSeries, start=c(yearStartSeries,quarterStartSeries), frequency=4)
					forecast_x_sa=c()
					forecast_x_sa[1]=x_sa_ts[length(x_sa_ts)] + forecast_x_sa_stationary_ts[1]
					for (i in 2:length(forecast_x_sa_stationary_ts))
					{
						forecast_x_sa[i]=forecast_x_sa[i-1] + forecast_x_sa_stationary_ts[i]
					}
					forecast_x_sa=ts(forecast_x_sa, start=c(year,quarter), frequency=4)
				} else {
					if(type=="Unit root and linear trend")
					{
						#to compute the forecasted series:
						#diff(x_sa)_forecast=intercept + betaTrend*trendF + forecast_x_sa_stationary_ts
						##1. we compute the forecasted first difference:
						###computing the intercept and betaTrend:
						trend=1:length(diff(saSeries))
						regression=lm(diff(saSeries)~trend, data.frame(trend, diff(saSeries)))
						intercept=summary(regression)$coeff[1,1]
						betaTrend=summary(regression)$coeff[2,1]

						### computing the forecasted first difference:
						trendF=(arimaEstimation$nobs+1):(arimaEstimation$nobs+nQuarters)
						forecast_diff_x_sa=intercept + betaTrend*trendF + forecast_x_sa_stationary_ts
						forecast_diff_x_sa_ts=ts(forecast_diff_x_sa, start=c(year,quarter), frequency=4)

						##2.we compute the forecasted_sa series:
						yearStartSeries=year-as.integer(length(saSeries)/4)
						q=(length(saSeries)/4-as.integer(length(saSeries)/4))/0.25
						quarterStartSeries=quarter-q
						x_sa_ts=ts(saSeries, start=c(yearStartSeries,quarterStartSeries), frequency=4)
						forecast_x_sa=c()
						forecast_x_sa[1]=x_sa_ts[length(x_sa_ts)] + forecast_diff_x_sa_ts[1]
						for (i in 2:length(forecast_diff_x_sa_ts))
						{
							forecast_x_sa[i]=forecast_x_sa[i-1] + forecast_diff_x_sa_ts[i]
						}
						forecast_x_sa=ts(forecast_x_sa, start=c(year,quarter), frequency=4)
					}
				}
			}
		}
		#Step 3: Add seasonality to the original forecasted series
		if(seasonalityType=="additive")
		{
			seasonalCoeff_ts=ts(rep(seasonalCoeff, as.integer(nQuarters/4 + 1)), start=c(year,1), frequency=4)
			forecast_x=forecast_x_sa + seasonalCoeff_ts
			forecast_x=as.data.frame(forecast_x)
		} else {
			if(seasonalityType=="multiplicative")
			{
				seasonalCoeff_ts=ts(rep(seasonalCoeff, as.integer(nQuarters/4 + 1)), start=c(year,1), frequency=4)
				forecast_x=forecast_x_sa*seasonalCoeff_ts
				forecast_x=as.data.frame(forecast_x)
			} else {
				if(seasonalityType=="none")
				{
					forecast_x=forecast_x_sa
					forecast_x=as.data.frame(forecast_x)
				}
			}
		}
	}
	rownames(forecast_x)=rownames(forecast_x_sa_stationary)
	colnames(forecast_x)[1]=model
	colnames(forecast_x)[2:ncol(forecast_x)]=colnames(forecast_x_sa_stationary)[2:ncol(forecast_x_sa_stationary)]
	forecast_x
}
