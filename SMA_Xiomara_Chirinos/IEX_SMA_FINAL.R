# Libraries
library(tidyverse)
library(jsonlite)
library(riingo)
library(zoo)
library(TTR)
library(lubridate)

# Tiingo Token
riingo_set_token("da346e990002a5a15fb2b86316e6cfceb454b30b")

# List of securities to trade in IEX
Symbol_List <- c(
  'AAPL', 'MSFT', 'AMZN', 'GOOGL', 'TSLA', 'FB', 'BABA', 'V', 'JNJ', 'WMT', 'NVDA', 'DIS',
  'PYPL', 'BAC', 'NFLX', 'INTC', 'NKE', 'CRM', 'KO', 'T', 'XOM', 'PFE', 'SNE', 'AZN', 'ZM',
  'SBUX', 'BA', 'AMD', 'ABNB', 'UBER', 'SQ', 'GE', 'SNAP', 'NIO', 'GM', 'MRNA', 'F', 'TWTR',
  'PTON', 'LUV', 'SIRI', 'DAL', 'NOK', 'CCL', 'RCL', 'NCLH', 'UAL', 'AAL', 'FUBO', 'JBLU',
  'CRON', 'SAVE', 'AMC', 'ACB', 'GPRO', 'OCGN', 'KOS', 'PSEC', 'IDEX', 'TXMD',
  'APHA', 'BNGO', 'BB', 'BLNK', 'CGC', 'CPRX', 'CCIV', 'GUSH', 'DKNG', 'SOLO',
  'ET', 'FCEL', 'GNUS', 'GEVO', 'HEXO', 'IDEX', 'INO', 'IVR', 'SLV', 'JNJ',
  'LI', 'MRO', 'MFA', 'MGM', 'NAKD', 'NNDM', 'NRZ', 'NKLA', 'OGI', 'PLTR',
  'PENN', 'PLUG', 'SNDL', 'TLRY', 'SPCE'
)

# Lists used in "For" loops
Symbol_History <- list()
SMA_Total <- list()
symbol_sell <- list()
symbol_buy <- list()
url_buy <- list()
url_sell <- list()
all_sells <- list()
all_buys <- list()


repeat 
{
  if((format(Sys.time(), format = "%H:%M:%S") >= "13:00:00") & (format(Sys.time(), format = "%H:%M:%S") < "14:30:00"))
  {
    
    # Retrieve the budget from profile during pre-market hours (9:00AM EST to 9:30AM EST)
    Budget <- (fromJSON("https://bigdataforall.com/stocks/?key=zidane&tiingo_key=da346e990002a5a15fb2b86316e6cfceb454b30b&request=profile")$budget)
    print(paste("Your budget has been updated during pre-market hours. You currently have", Budget))
    Sys.sleep(60)
    
  } else if((format(Sys.time(), format = "%H:%M:%S") >= "14:30:00") & (format(Sys.time(), format = "%H:%M:%S") < "20:45:00"))
    
  {
    
    print("Your loop is starting.")
    
    for(a in Symbol_List)
    {
      # Create history of all symbols. by minute, going back 5 days
      Symbol_History[[a]] <- riingo_iex_prices(a, resample_frequency = "1min", start_date = (Sys.Date() - 5), end_date = Sys.time())
    }
    
    # Binding lists
    Symbol_History_Final <- unique(do.call("rbind", Symbol_History))
    
    for (b in Symbol_List)
    {
      
      # Creating simple moving average (SMA) lists. See ReadMe for details.
      SMA_Total[[b]] <- Symbol_History_Final %>%
        filter(ticker == b) %>%
        mutate(price_avg = close) %>%
        select(ticker, date, price_avg) %>%
        group_by(ticker) %>%
        arrange(date) %>%
        mutate(avg_roll_30 = SMA(price_avg, n = 30, align = "right")) %>%
        mutate(avg_roll_90 = SMA(price_avg, n = 90, align = "right")) %>%
        mutate(previous_roll_90 = lag(avg_roll_90, order_by = date)) %>%
        mutate(previous_roll_30 = lag(avg_roll_30, order_by = date)) %>%
        filter(date >= (Sys.Date() - 4)) %>%
        tail(1) %>%
        mutate(action = ifelse((avg_roll_30 > avg_roll_90) & (previous_roll_30 <= previous_roll_90), "SELL", 
                               ifelse((avg_roll_30 < avg_roll_90) & (previous_roll_30 >= previous_roll_90), "BUY", "NOTHING")))
    }
    
    # Binding SMA lists.
    SMA_Total_Final <- unique(do.call("rbind", SMA_Total)) %>%
      filter((action == "BUY") | (action == "SELL")) %>%
      arrange(date)
    
    SMA_Total_Final[nrow(SMA_Total_Final)+1,] <- NA
    
    # Generating a count of how many signals the model generated in one minute
    Signals_per_minute <- select(SMA_Total_Final, date, action) %>%
      select(date) %>%
      group_by(date) %>%
      summarize(count = n()) %>%
      tail(1)
    
    Signals_per_minute[nrow(Signals_per_minute)+1,] <- NA
    
    # Creating a master list of succesful transactions
    Master_Log <- fromJSON("https://bigdataforall.com/stocks/?key=zidane&tiingo_key=da346e990002a5a15fb2b86316e6cfceb454b30b&request=log")$log%>%
      filter(result == "success") %>%
      select(id, symbol)
    
    # For loop for minutes where there are at most two signals
    if (Signals_per_minute$count[1] <= 2 | is.na(Signals_per_minute$count[1]))
    {
      
      for (c in (1:nrow(SMA_Total_Final))) 
      {
        
        sym <- SMA_Total_Final$ticker[c]
        
        # Count of signals per symbol
        ticker_count <- select(Master_Log, id, symbol) %>%
          filter(symbol == sym) %>%
          group_by(id) %>%
          summarize(count = n())
        
        ticker_count[nrow(ticker_count)+1,] <- NA
        
        # Only for symbols with < 10 transactions in the day
        if((ticker_count$count[1] < 10) | (is.na(ticker_count$count[1])))
        {
          
          # Filtering NA in the ticker list
          if(!is.na(SMA_Total_Final$ticker[c]))
          {
            
            # Signal = Buy
            if(SMA_Total_Final$action[c] == "BUY")
            {
              
              # Comparing budget to price of 2 stocks
              if(Budget >= (SMA_Total_Final$price_avg[c]*2))
              {
                url_buy_base <- "https://bigdataforall.com/stocks/?key=zidane&tiingo_key=da346e990002a5a15fb2b86316e6cfceb454b30b&request=buy"
                quantity_buy <- c(2)
                
                url_buy[[c]] <- paste(url_buy_base, "&symbol=", sym, "&quantity=", quantity_buy, sep = "")
                
                all_buys[[c]] <- fromJSON(url_buy[[c]])
                
                buy_sucess_log <- fromJSON("https://bigdataforall.com/stocks/?key=zidane&tiingo_key=da346e990002a5a15fb2b86316e6cfceb454b30b&request=log")$log%>%
                  select(symbol, request, quantity, result, details, date, budget) %>%
                  filter(request == "buy", result == "success")
                
                # Update budget
                budget_buy_log <- fromJSON("https://bigdataforall.com/stocks/?key=zidane&tiingo_key=da346e990002a5a15fb2b86316e6cfceb454b30b&request=log")$log%>%
                  mutate(id = as.integer(id), budget = as.double(budget)) %>%
                  select(id, budget) %>%
                  filter(id == max(id))$budget
                
                Budget <- budget_buy_log
                
                print(paste("Buying",  quantity_buy, " ", sym, "stocks. You have a budget of", Budget, " remaining."))
                
              }
              
              # Signal = Sell
            } else if(SMA_Total_Final$action[c] == "SELL")
            {
              
              # Checking for symbol in master log
              if(sym %in% Master_Log$symbol)
              {
                
                url_sell_base <- "https://bigdataforall.com/stocks/?key=zidane&tiingo_key=da346e990002a5a15fb2b86316e6cfceb454b30b&request=sell"
                quantity_sell <- c(1)
                
                url_sell[[c]] <- paste(url_sell_base, "&symbol=", sym, "&quantity=", quantity_sell, sep = "")
                
                all_sells[[c]] <- fromJSON(url_sell[[c]])
                
                sell_sucess_log <- fromJSON("https://bigdataforall.com/stocks/?key=zidane&tiingo_key=da346e990002a5a15fb2b86316e6cfceb454b30b&request=log")$log%>%
                  select(symbol, request, quantity, result, details, date, budget) %>%
                  filter(request == "sell", result == "success")
                
                # Updating budget
                budget_sell_log <- fromJSON("https://bigdataforall.com/stocks/?key=zidane&tiingo_key=da346e990002a5a15fb2b86316e6cfceb454b30b&request=log")$log%>%
                  mutate(id = as.integer(id), budget = as.double(budget)) %>%
                  select(id, budget) %>%
                  filter(id == max(id))$budget
                
                Budget <- budget_sell_log
                
                print(paste("Selling",  quantity_sell, " ", sym, "stocks. You have a budget of", Budget, " remaining."))
                
                
                
              }
              
            }
          }
        }
      }
      
      # More than two signals per minute.
    } else
    {
      # Choose the top 2 transactions in that minute
      SMA_Total_Final <- SMA_Total_Final %>%
        filter(action == 'BUY' | action == 'SELL')
      head(2)
      
      SMA_Total_Final[nrow(SMA_Total_Final)+1,] <- NA
      
      for (d in (1:nrow(SMA_Total_Final))) 
      {
        
        sym <- SMA_Total_Final$ticker[d]
        
        # Creating count of tickers again
        ticker_count <- select(Master_Log, id, symbol) %>%
          filter(symbol == sym) %>%
          group_by(id) %>%
          summarize(count = n())
        
        ticker_count[nrow(ticker_count)+1,] <- NA
        
        # Ticker count less than 10 for that day
        if((ticker_count$count < 10) | (is.na(ticker_count$count)))
        {
          
          if(!is.na(SMA_Total_Final$ticker[d])){
            
            # Signal = Buy
            if(SMA_Total_Final$action[d] == "BUY")
            {
              
              if(Budget >= SMA_Total_Final$price_avg[d]*2)
              {
                url_buy_base <- "https://bigdataforall.com/stocks/?key=zidane&tiingo_key=da346e990002a5a15fb2b86316e6cfceb454b30b&request=buy"
                quantity_buy <- c(2)
                
                url_buy[[d]] <- paste(url_buy_base, "&symbol=", sym, "&quantity=", quantity_buy, sep = "")
                
                all_buys[[d]] <- fromJSON(url_buy[[d]])
                
                buy_sucess_log <- fromJSON("https://bigdataforall.com/stocks/?key=zidane&tiingo_key=da346e990002a5a15fb2b86316e6cfceb454b30b&request=log")$log%>%
                  select(symbol, request, quantity, result, details, date, budget) %>%
                  filter(request == "buy", result == "success")
                
                # Update budget
                budget_buy_log <- fromJSON("https://bigdataforall.com/stocks/?key=zidane&tiingo_key=da346e990002a5a15fb2b86316e6cfceb454b30b&request=log")$log%>%
                  mutate(id = as.integer(id), budget = as.double(budget)) %>%
                  select(id, budget) %>%
                  filter(id == max(id))$budget
                
                Budget <- budget_buy_log
                
                print(paste("Buying",  quantity_buy, " ", sym, "stocks. You have a budget of", Budget, " remaining."))
                
              }
              
              # Signal = Sell
            } else if(SMA_Total_Final$action[d] == "SELL")
            {
              
              # Check if symbol is in log
              if(sym %in% Master_Log$symbol)
              {
                
                url_sell_base <- "https://bigdataforall.com/stocks/?key=zidane&tiingo_key=da346e990002a5a15fb2b86316e6cfceb454b30b&request=sell"
                quantity_sell <- c(4)
                
                url_sell[[d]] <- paste(url_sell_base, "&symbol=", sym, "&quantity=", quantity_sell, sep = "")
                
                all_sells[[d]] <- fromJSON(url_sell[[d]])
                
                sell_sucess_log <- fromJSON("https://bigdataforall.com/stocks/?key=zidane&tiingo_key=da346e990002a5a15fb2b86316e6cfceb454b30b&request=log")$log%>%
                  select(symbol, request, quantity, result, details, date, budget) %>%
                  filter(request == "sell", result == "success")
                
                # Update budget
                budget_sell_log <- fromJSON("https://bigdataforall.com/stocks/?key=zidane&tiingo_key=da346e990002a5a15fb2b86316e6cfceb454b30b&request=log")$log%>%
                  mutate(id = as.integer(id), budget = as.double(budget)) %>%
                  select(id, budget) %>%
                  filter(id == max(id))$budget
                
                Budget <- budget_sell_log
                
                print(paste("Selling",  quantity_sell, " ", sym, "stocks. You have a budget of", Budget, " remaining."))
                
                
              }
              
            }
          }
        }
      }
      
    }
    
    # Updating budget
    Budget <- fromJSON("https://bigdataforall.com/stocks/?key=zidane&tiingo_key=da346e990002a5a15fb2b86316e6cfceb454b30b&request=log")$log%>%
      mutate(id = as.integer(id), budget = as.double(budget)) %>%
      select(id, budget) %>%
      filter(id == max(id))$budget
    
    print(paste("Sleeping for 1 minute... Your remaining cash is", Budget))
    
    Sys.sleep(60)
    
  } else if((format(Sys.time(), format = "%H:%M:%S") >= "20:45:00") & (format(Sys.time(), format = "%H:%M:%S") < "20:59:00")){
    
    # Buy ADT stock based on remaining budget
    if(Budget > 20){
      ADT_Stock <- riingo_iex_prices('ADT', resample_frequency = "1min", start_date = Sys.time(), end_date = Sys.time()) %>%
        arrange(date) %>%
        tail(1)
      
      ADT_Stock_Purchase <- Budget %/% ADT_Stock$close
      
      url_buy_2 <- paste(url_buy_base, "&symbol=", "ADT", "&quantity=", ADT_Stock_Purchase, sep = "")
      
      fromJSON(url_buy_2)
      
      # Update budget
      Budget <- fromJSON("https://bigdataforall.com/stocks/?key=zidane&tiingo_key=da346e990002a5a15fb2b86316e6cfceb454b30b&request=log")$log%>%
        mutate(id = as.integer(id), budget = as.double(budget)) %>%
        select(id, budget) %>%
        filter(id == max(id))$budget
      
      
      print(paste("Market closing soon! Buying",  ADT_Stock_Purchase, "ADT", "stocks. You have a remaining budget of", Budget))
      
    } else {
      
      print(paste("Market is closing soon! Not enough leftover cash to buy ADT! You have a remaining budget of", Budget))
      
    }
    
  } else {
    
    print("Sleeping for 1 hour...")
    
    Sys.sleep(3600)
  }
  
}