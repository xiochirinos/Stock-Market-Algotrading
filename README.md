# Stock Market Algotrading

***

## Authors

Rene Villarreal.
Xiomara Chirinos. 
Mariah Bastos.

***

## Overview

The purpose of this project was to create a day-trading algorithm in the IEX market given a specific set of instructions and restrictions. Each team member was given $100,000 and had the following restrictions in order to avoid penalties: 


Trading must be done automatically from an EC2 instance. 
Can only do two transactions per minute. 
At least 250 successful trades per day. 
Maximum of 500 successful trades per day. 
Trading hours are 9:30AM EST to 4:00PM EST. 
Only ten trades per symbol per day. 
Uninvested cash is wiped at the end of each trading day. 

The penalties for having less than 250 transactions were $100 per missed transaction, and the penalties for failed transactions were $1,000 each. 

Two APIs were used in our codes: an API for accessing big data for all, and an API for accessing Tiingo.

***

## Approach

*Exponential Moving Average:*
The first model we used was an exponential moving average. We created two moving averages, a shorter time period (30 minutes) and a longer time period (90 minutes). Our approach using this model was simple: whenever the shorter moving average crossed the longer moving average going up, we would generate a sell signal. When the shorter moving average crossed the longer moving average going down, we would generate a buy signal. This approach is widely used in long-term trading, as it tends to show trends and when a market is bearish or bullish. In day-trading, the rules are reversed because the moving averages move very closely with the real time data. So in a long-term trading when a shorter average crosses the longer average, it tends to mean that the stock is bullish and we should buy. In day trading, it means the stock is at a higher price than when you last bought it so we would sell instead of buy. 


*Simple Moving Average:* 
Similar to the one above, it generates the same signals when a shorter moving average crosses the longer moving average. The main difference between these two approaches is that the simple moving averages weighs all data points the same, while an exponential moving average gives more weight to more recent data points. 


*Scalping:* 
This is a common approach to day-trading. The main purpose of this model is to generate small profits whenever the stock rises above the buying price. Generally, the profits are not big, but the idea is that with the sum of small profits you can eventually make enough. This model carries lower risk than the exponential moving average and the simple moving average because it is strictly based on the price of the stock at the moment, and not it's history. 

***

## Instructions

In order to run the codes in our folders, you will need the following libraries: 

tidyverse: with this library we used mostly dplyr for cleaning up our data. 
jsonlite: this is mostly used to read the JSON messsages whenever we execute a trade and to access our budget. 
riingo: we used this package to pull data from Tiingo using our API key. 
zoo: we included this package in the library set to use the lag function. 
TTR: this package was used to create the simple and exponential moving averages. 
lubridate: manipulate dates and timestamps. 

The R scripts can be taken directly from our folders and run in R. Please note that since the market is closed and we currently all have a negative balance, you will not be able to execute a successful buy or sell.
