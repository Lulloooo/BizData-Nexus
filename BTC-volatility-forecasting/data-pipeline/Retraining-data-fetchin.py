
################


################

#import packages
from pathlib import Path
import pandas as pd
import requests
from pytrends.request import TrendReq
from datetime import datetime, timedelta
import time

#directories
base_dir = Path(__file__).resolve().parents[1]   # BTC-volatility-forecasting/
data_dir = base_dir / "data"/ "training_data"


###### BASELINE REGRESSORS & TARGET FUNCTION
##regressors
X_start = pd.read_csv(data_dir/ "X_baseline.csv", parse_dates=["date"])
#sort by date
X_start = X_start.sort_values("date")
#show last date
last_date = X_start["date"].max()
##target
y_start= pd.read_csv(data_dir/ "y_baseline.csv")
#debugging
if X_start is not None:
    print("Regressors loaded") 
if y_start is not None:
    print("Target loaded.")
#show last date
print(f"last date: {last_date}")



###### NEW DATA FETCHING (last_date to now)
#define start date
start = last_date + timedelta(days=1)
###OHLCV
def update_ohlcv_coingecko(symbol = "bitcoin", vs_currency = "usd"):
    #define start and end date
    start = pd.Timestamp(last_date) + pd.Timedelta(days=1)
    end = datetime.utcnow()
    
    #if start is > end halt it
    if start >= end:
        print("🟢 OHLCV already up to date")
        return pd.DataFrame()
    #else pharse the last dates
    else:
        #define the endpoint
        url = f"https://api.coingecko.com/api/v3/coins/{symbol}/market_chart/range"
        params = {
            "vs_currency": vs_currency,
            "from": int(start.timestamp()),
            "to": int(end.timestamp())
        }
        #make the request
        r = requests.get(url, params=params, timeout=20)
        r.raise_for_status()
        data = r.json()
        #get prices & volumne
        prices = pd.DataFrame(data["prices"], columns=["timestamp", "close"])
        volumes = pd.DataFrame(data["total_volumes"], columns=["timestamp", "volume"])
        ohlcv = prices.merge(volumes, on="timestamp")
        #set index & timestamp
        ohlcv["timestamp"] = pd.to_datetime(ohlcv["timestamp"], unit="ms")
        ohlcv.set_index("timestamp", inplace=True)
        #normalize timezone
        ohlcv.index = pd.to_datetime(ohlcv.index).tz_localize(None).normalize()
        # OHLC resample
        ohlcv = ohlcv.resample("1D").agg(
            open=("close", "first"),
            high=("close", "max"),
            low=("close", "min"),
            close=("close", "last"),
            volume=("volume", "sum")
        ).dropna()
    return ohlcv
###FG INDEX
def fear_greed_index():
    #API to get all historical data in JSON
    url = "https://api.alternative.me/fng/?limit=0&format=json"
    #make an HTTP get req
    response = requests.get(url)
    #convert http in json and extract "data"
    data = response.json()["data"]
    #convert data (a list of dictionaries) into a df 
    fg_index = pd.DataFrame(data)
    #change timestamp to datetime format in seconds
    fg_index["timestamp"] = pd.to_datetime(fg_index["timestamp"], unit = "s")
    #set timestamp as the index
    fg_index.set_index("timestamp", inplace = True)
    #normalize it
    fg_index.index = pd.to_datetime(fg_index.index).tz_localize(None).normalize()
    #select only the value column and change it into float
    fg_index = fg_index[["value"]].astype(float)
    #rename the column
    fg_index.rename(columns = {"value" : "fear&greed_index",
                               "timestamp" : "date"},
                    inplace = True)
    #sort the index
    fg_index = fg_index.sort_index()
    #define end date as today
    end = datetime.utcnow().date()
    #define start date
    start = last_date + timedelta(days=1)
    #slice time range
    fg_index = fg_index.loc[start:end]
    
    #return the final df
    return fg_index

### GOOGLE TRENDS
def fetch_daily_trends(keyword="Bitcoin", start_date=None, sleep_time=5):
    if start_date is None:
        start_date = last_date + timedelta(days=1)
    pytrends = TrendReq()
    #define end_date
    end_date = pd.Timestamp.today().strftime("%Y-%m-%d")
    #define the timeframe
    timeframe = f"{start_date} {end_date}"
    try:
        #build payload
        pytrends.build_payload([keyword], timeframe=timeframe)
        #store in a df
        df = pytrends.interest_over_time()
        
        if df.empty:
            return pd.DataFrame()
        #rename column
        df = df.rename(columns={keyword: "google_trends"})
        #drop columns is partial
        df = df.drop(columns=["isPartial"])
        #normlize data
        df.index = pd.to_datetime(df.index).tz_localize(None).normalize()
        #sort the index
        df = df.sort_index()
        #return the df
        return df
    #raise errors
    except Exception as e:
        #error 429
        if "429" in str(e):
            print("❌ Error 429 (Too Many Requests). Try increasing sleep_time or using proxies.")
        #other errors
        else:
            print(f"⚠️ Error retrieving Google Trends data: {e}")
        #return df 
        return pd.DataFrame()
    

####### DATA GATHERING & MERGING
#ohlcv
ohlcv = update_ohlcv_coingecko()
#fg_index 
fg_index = fear_greed_index()
#google trend
trend = fetch_daily_trends()
### MERGING
# Merge fg index and trend on index (date)
index_and_trends = fg_index.join(trend, how="outer")
#Add ohlcv (on index, i.e date)
df_updated = ohlcv.join(index_and_trends, how="outer")


#DATASET UPDATING
#concat the most recent part to the df
X_full = pd.concat([X_start.set_index("date"), df_updated])
#if there are some data duplicates, keep just one
X_full = X_full[~X_full.index.duplicated(keep="last")]
#show it
print("df_upTodate preview:")
print(X_full.head(10))
#Save the updated datase
X_full.to_csv(data_dir/ "X_upToDate.csv")