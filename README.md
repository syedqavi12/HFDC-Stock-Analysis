# HFDC-Stock-Analysis

Group Project for Time Series Analysis Forecasting

### Description 

HDFC.csv: Main dataset file used for project

Time Series Final Report: Detailed Group report of the findings from stock price analysis
My part- Analysing the Close variable of HDFC Stock market pg. 7-9 and pg. 52

FP_hdfc: R code used to analyze HDFC close price 

FP_Presentation_SyedQavi: Visuals generated from R code output

### Overview

I analyzed this variable and fitted the variable vs time, transformed it using Box Cox transformation and got the best ARIMA model by analyzing the ACF, PACF, eacf and the residuals for white noise then backtesting the different models to get the best fit model. I did a forecast of the ARIMA model to see the forecast behavior. 

Then I transformed the original series into a log return so I can model the volatility of the returns. I fitted the a GARCH(1,1) model with the ARIMA model that I had previously calculated, then I analyzed the alpha and beta parameters or the lagged squared residuals and the lagged variance. I plotted different residual plots of the GARCH model and forecast along with the backtest to see if the GARCH model is a good fit. 

Then I fitted a ugarch model with the specification to see if I can improve the model in terms of goodness of fit. Also ran the backtest version of the ugarch to see if it perfomed better than the standard garch fit model.

