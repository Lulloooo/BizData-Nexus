import requests
import pandas as pd
from pathlib import Path
from datetime import datetime, timedelta

# -------------------
# CONFIG
# -------------------
<<<<<<< HEAD
SYMBOL = "bitcoin"
VS_CURRENCY = "usd"
OUTPUT_PATH = Path("BTCVolatility-data/volatility_data.csv")
N_DAYS = 90  # Number of days to fetch
=======
SYMBOL = "BTC-USDT"
TIMEFRAME = "1d"
OUTPUT_PATH = Path(
    "BTC-Volatility-forecasting/BTC-volatility-data/btc-data.csv"
)
>>>>>>> 655c1f4a370811e05a0d8de2030c50c7e8946484

# -------------------
# FETCH DATA
# -------------------
<<<<<<< HEAD
def fetch_ohlcv_coingecko(symbol, vs_currency, n_days=90):
    end = datetime.utcnow()
    start = end - timedelta(days=n_days)
=======
def fetch_ohlcv(symbol, timeframe, since=None):
    exchange = ccxt.okx({"enableRateLimit": True,
                         "options": {"defaultType": "spot"}
                        })
    limit = 1000
    all_data = []
>>>>>>> 655c1f4a370811e05a0d8de2030c50c7e8946484

    url = f"https://api.coingecko.com/api/v3/coins/{symbol}/market_chart/range"
    params = {
        "vs_currency": vs_currency,
        "from": int(start.timestamp()),
        "to": int(end.timestamp())
    }

    r = requests.get(url, params=params, timeout=20)
    r.raise_for_status()
    data = r.json()

    prices = pd.DataFrame(data["prices"], columns=["timestamp", "close"])
    volumes = pd.DataFrame(data["total_volumes"], columns=["timestamp", "volume"])
    df = prices.merge(volumes, on="timestamp")

    df["timestamp"] = pd.to_datetime(df["timestamp"], unit="ms")
    df.set_index("timestamp", inplace=True)

    # OHLC resample
    df = df.resample("1D").agg(
        open=("close", "first"),
        high=("close", "max"),
        low=("close", "min"),
        close=("close", "last"),
        volume=("volume", "sum")
    ).dropna()

    return df

# -------------------
# SAVE TO CSV
# -------------------
def update_backup():
    OUTPUT_PATH.parent.mkdir(exist_ok=True, parents=True)

    if OUTPUT_PATH.exists():
        existing = pd.read_csv(OUTPUT_PATH, parse_dates=["timestamp"], index_col="timestamp")
        last_ts = existing.index[-1]
        df_new = fetch_ohlcv_coingecko(SYMBOL, VS_CURRENCY, N_DAYS)
        df = pd.concat([existing, df_new]).drop_duplicates()
    else:
        df = fetch_ohlcv_coingecko(SYMBOL, VS_CURRENCY, N_DAYS)

    df = df.sort_index()
    df.to_csv(OUTPUT_PATH)
    print(f"✅ Saved backup: {len(df)} rows ({df.index.min()} → {df.index.max()})")

# -------------------
# RUN
# -------------------
if __name__ == "__main__":
    update_backup()
