from datetime import datetime, timedelta
import os

from dotenv import load_dotenv
import matplotlib.pyplot as plt
import pandas as pd
from twelvedata import TDClient


load_dotenv()
API_KEY = os.getenv('TWELVE_DATA_API_KEY')
td = TDClient(apikey=API_KEY)
TOMORROW = (datetime.now() + timedelta(1)).date()


def main():
    print(TOMORROW)
    df = read_or_download()
    print(df.head())
    print(df.tail())
    plt.plot(df.close)
    plt.yscale('log')
    plt.show()
    


def read_or_download():
    filename = 'sample.csv'
    try:
        df = pd.read_csv(filename)
        df['datetime'] = pd.to_datetime(df['datetime']).dt.date
        df.set_index('datetime', drop=True, inplace=True)
    except FileNotFoundError:
        df = td.time_series(
            symbol='AAPL',
            interval='1day',
            start_date='2004-01-01',
            end_date=TOMORROW,
            outputsize=5000,
            timezone='Exchange',
            order='asc'
        ).as_pandas()
        df.to_csv(filename)
    return df


if __name__ == '__main__':
    main()
