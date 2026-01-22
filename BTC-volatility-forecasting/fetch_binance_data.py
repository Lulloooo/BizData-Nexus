import ccxt
import pandas as pd
from pathlib import Path

# -------------------
# CONFIG
# -------------------
SYMBOL = "BTC/USDT"
TIMEFRAME = "1d"
OUTPUT_PATH = Path(
    "BTC-Volatility-forecasting/BTC-volatility-data/btc_data.csv"
)

# -------------------
# FETCH FUNCTION
# -------------------
def fetch_ohlcv(symbol, timeframe, since=None):
    exchange = ccxt.okx({"enableRateLimit": True})
    limit = 1000
    all_data = []

    while True:
        data = exchange.fetch_ohlcv(
            symbol,
            timeframe=timeframe,
            since=since,
            limit=limit
        )

        if not data:
            break

        all_data.extend(data)
        since = data[-1][0] + 1

        if len(data) < limit:
            break

    return all_data

# -------------------
# MAIN LOGIC
# -------------------
if OUTPUT_PATH.exists():
    # Incremental update
    existing = pd.read_csv(OUTPUT_PATH, parse_dates=["timestamp"], index_col="timestamp")
    last_ts = int(existing.index[-1].timestamp() * 1000)

    print("🔄 Updating from:", existing.index[-1])

    new_data = fetch_ohlcv(SYMBOL, TIMEFRAME, since=last_ts)

    df_new = pd.DataFrame(
        new_data,
        columns=["timestamp", "open", "high", "low", "close", "volume"]
    )

    if not df_new.empty:
        df_new["timestamp"] = pd.to_datetime(df_new["timestamp"], unit="ms")
        df_new.set_index("timestamp", inplace=True)
        df = pd.concat([existing, df_new]).drop_duplicates()
    else:
        df = existing

else:
    # Full backfill (run once)
    print("⬇️ Full historical download")
    start_date = "2017-08-01T00:00:00Z"
    exchange = ccxt.binance()
    since = exchange.parse8601(start_date)

    data = fetch_ohlcv(SYMBOL, TIMEFRAME, since=since)

    df = pd.DataFrame(
        data,
        columns=["timestamp", "open", "high", "low", "close", "volume"]
    )
    df["timestamp"] = pd.to_datetime(df["timestamp"], unit="ms")
    df.set_index("timestamp", inplace=True)

# -------------------
# SAVE
# -------------------
df = df.sort_index()
df.to_csv(OUTPUT_PATH)

print(f"✅ BTC data saved ({len(df)} rows)")
print("Range:", df.index.min(), "→", df.index.max())
