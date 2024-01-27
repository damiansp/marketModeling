from datetime import datetime, timedelta
import os

import pandas as pd


def get_adjusted_date():
    'Treat weekend dates as preceding Friday'
    NOW = datetime.now()
    weekday = NOW.weekday()
    if weekday == 5:    # Sat
        return NOW - timedelta(1)
    elif weekday == 6:  # Sun
        return NOW - timedelta(2)
    return NOW


HOME = os.environ['HOME']
DOWNLOADS = f'{HOME}/Downloads'
now = get_adjusted_date()
TODAY = now.date()
TOMORROW = (now + timedelta(1)).date()


class HoldingsAppender:
    def __init__(self, stock_metrics):
        if 'stock' in list(stock_metrics):
            stock_metrics.set_index('stock', inplace=True)
        self.stock_metrics = stock_metrics
        self.SYMBOLS = list(set(stock_metrics.index.tolist()))
        self.binder = pd.DataFrame(index=self.SYMBOLS)

    def append_holdings(self):
        etrade, fidelity, schwab = self._upload_files()
        out = pd.concat([self.binder, etrade, schwab], axis=1)
        for v in fidelity.values():
            out = pd.concat([out, v], axis=1)
        sims = self._upload_sims()
        simcols = list(sims.columns)
        dm = self._upload_dm()
        actual_cols = ['et', 'schwab', 'rollover', 'roth', 'simple']
        out = pd.concat([out, sims, dm], axis=1)
        out = out.astype(float).fillna(0).round().astype(int)
        out_cols = actual_cols + simcols + ['dm']
        out.columns = out_cols
        for col in out_cols + ['fid']:
            if col in list(self.stock_metrics):
                self.stock_metrics.drop(columns=[col], inplace=True)
        out['fid'] = out.rollover + out.roth + out.simple
        metrics = pd.concat([self.stock_metrics, out], axis=1)
        metrics['Owned'] = metrics[actual_cols].sum(axis=1)
        return metrics

    def _upload_files(self):
        etrade = self._upload_etrade()
        fidelity = self._upload_fidelity()
        schwab = self._upload_schwab()
        return etrade, fidelity, schwab
        
    def _upload_etrade(self):
        filename = 'Positions.csv'
        path = f'{DOWNLOADS}/{filename}'
        self._preclean(path)
        print('Uploading E*Trade data...')
        etrade = (
            pd
            .read_csv(path, index_col=0, skiprows=1)[['Market Value']]
            .rename(columns={'Market Value': 'ET'}))
        etrade.index = map(lambda x: x.split()[0], etrade.index)
        drops = [d for d in ['FNRG', 'Portfolio', 'Cash'] if d in etrade.index]
        etrade.drop(index=drops, inplace=True)
        if len(etrade[etrade.ET == '--']):
            raise ValueError(
                f'Missing price data in ETrade file ({filename}). Correct.')
        if not self._symbols_are_valid(etrade):
            raise ValueError('E*Trade file has unexpected symbol')
        return etrade

    @staticmethod
    def _preclean(path):
        print(f'Pre-cleaning {path}...')
        with open(path, 'r') as fi:
            data_in = fi.readlines()
            with open(path, 'w') as fo:
                for line in data_in:
                    fields = line.split(',')
                    if len(fields) > 2 and fields[2] == '"--"':
                        continue
                    else:
                        fo.write(line)

    def _upload_fidelity(self):
        print('Uploading Fidelity data...')
        today = datetime.strftime(TODAY, '%b-%d-%Y')
        filename = f'Portfolio_Positions_{today}.csv'
        print('Looking for Fidelity file:', filename)
        try:
            fidelity = pd.read_csv(f'{DOWNLOADS}/{filename}')
        except FileNotFoundError:
            tomorrow = datetime.strftime(TOMORROW, '%b-%d-%Y')
            filename = filename.replace(today, tomorrow)
            fidelity = pd.read_csv(f'{DOWNLOADS}/{filename}')
        fidelity = (
            fidelity[['Account Name', 'Symbol', 'Current Value']].dropna())
        fidelity['Current Value'] = (
            fidelity['Current Value']
            .apply(self._convert_value)
            .fillna(0)
            .astype(int))
        fidelity = fidelity[
            ((fidelity.Symbol != 'SPAXX**')
             & (fidelity.Symbol != 'Pending Activity'))]
        fidelity = self._separate_accounts(fidelity)
        return fidelity

    def _separate_accounts(self, fid):
        accounts = {
            account_name: fid[fid['Account Name'] == account_name]
            for account_name in fid['Account Name'].unique()}
        for acct in accounts:
            accounts[acct].index = accounts[acct].Symbol
            accounts[acct] = (
                accounts[acct]
                .rename(columns={'Current Value': acct})
                .drop(columns=['Account Name', 'Symbol']))
        for k, v in accounts.items():
            if not self._symbols_are_valid(v):
                raise ValueError(f'Fidelity {k} data has unexpected symbol')
        return accounts

    def _upload_schwab(self):
        print('Uploading Schwab data...')
        path_start = f'PCRA_Custodial-Positions-{str(TODAY)}'
        print(f'Looking for Schwab file: {path_start}...')
        filename = [
            f for f in os.listdir(DOWNLOADS) if f.startswith(path_start)
        ][0]
        path = f'{DOWNLOADS}/{filename}'
        schwab = self._parse_schwab(path)
        if not self._symbols_are_valid(schwab):
            raise ValueError('Unexpected symbol in Schwab file')
        return schwab

    @staticmethod
    def _parse_schwab(path):
        data = []
        inds = []
        is_header = True
        with open(path, 'r') as f:
            for line in f:
                if line.startswith('"Cash') or line.startswith('"Account'):
                    continue
                if not is_header:
                    cols = line.split(',"')
                    symbol = cols[0].strip('""')
                    amt = float(cols[6].strip('"$'))
                    inds.append(symbol)
                    data.append(amt)
                if line.startswith('"Symbol"'):
                    is_header = False
        return pd.DataFrame({'Schwab': data}, index=inds)

    def _upload_sims(self):
        print('Uploading simulation data...')
        #files = [f for f in os.listdir(DOWNLOADS) if f.startswith('Holdings')]
        files = [
            f'Holdings - Damian Satterthwaite-Phillips{x}.csv' for
            x in ['', '(1)', '(2)']]
        print(f'Found {len(files)} sim files.')
        dfs = []
        for f in files:
            df = pd.read_csv(
                f'{DOWNLOADS}/{f}', index_col=0, usecols=['Symbol', 'Value'])
            df.Value = df.Value.str.replace(',', '').str[1:].astype(float)
            drop = [sym for sym in df.index if sym not in self.SYMBOLS]
            df.drop(drop, inplace=True)
            dfs.append(df)
        out = pd.concat(dfs, axis=1)
        out.columns = [f'sim{i}' for i in range(1, len(files) + 1)]
        return out

    def _upload_dm(self):
        if 'Dongmei.csv' not in os.listdir(DOWNLOADS):
            return pd.DataFrame(
                {'dm': [0] * len(self.SYMBOLS)}, index=self.SYMBOLS)
        dm = pd.read_csv(
            f'{DOWNLOADS}/Dongmei.csv',
            index_col=0,
            usecols=['Position', 'Market Value']
        ).rename(columns={'Market Value': 'dm'})
        dm.index = [x.replace(' shares', '') for x in dm.index]
        return dm
        
    @staticmethod
    def _convert_value(s):
        return round(float(s.replace('$', '').replace(',', '')))
        
    def _symbols_are_valid(self, df):
        for symbol in df.index.unique():
            if symbol not in self.SYMBOLS:
                print(f'{symbol} not in SYMBOLS')
                return False
        return True


if __name__ == '__main__':
    pass
    # Test uploads
    #DATA = '../data_new'
    #STOCK_METRICS = f'{DATA}/stock_metrics.csv'
    #stock_metrics = pd.read_csv(STOCK_METRICS, index_col=0)
    #appender = HoldingsAppender(stock_metrics)
    #appender._upload_etrade()
