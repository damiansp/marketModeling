from datetime import datetime, timedelta
import os

import pandas as pd


def get_adjusted_date():
    'Treat weekend dates as preceding Friday'
    now = datetime.now()
    weekday = now.weekday()
    if weekday == 5:    # Sat
        return now - timedelta(1)
    elif weekday == 6:  # Sun
        return now - timedelta(2)
    return now


HOME = os.environ['HOME']
DOWNLOADS = f'{HOME}/Downloads'
NOW = get_adjusted_date()
TODAY = NOW.date()
TOMORROW = (NOW + timedelta(1)).date()


class HoldingsLoader:
    def load(self):
        etrade = self._upload_etrade()
        fidelity = self._upload_fidelity()
        schwab = self._upload_schwab()
        #dm = self._upload_dm()
        dm = self._upload_etrade(is_dm=True)
        sims = self._upload_sims()
        out = pd.concat(
            [etrade, fidelity, schwab, dm, sims], axis=1
        ).sort_index()
        out = out.astype(float).fillna(0)
        out['owned'] = out.et + out.fid + out.schwab
        out = out.round().astype(int)
        return out

    def _upload_etrade(self, is_dm=False):
        filename = 'Positions(1).csv'  if is_dm else 'Positions.csv'
        path = f'{DOWNLOADS}/{filename}'
        self._preclean_etrade(path)
        print('Uploading E*Trade data...')
        val_col = 'dm' if is_dm else 'et'
        etrade = (
            pd
            .read_csv(path, index_col=0, skiprows=1)[['Market Value']]
            .rename(columns={'Market Value': val_col}))
        etrade.index = map(lambda x: x.split()[0], etrade.index)
        drops = [d for d in ['FNRG', 'Portfolio', 'Cash'] if d in etrade.index]
        etrade.drop(index=drops, inplace=True)
        if len(etrade[etrade[val_col] == '--']):
            raise ValueError(
                f'Missing price data in ETrade file ({filename}). Correct.')
        etrade[val_col] = etrade[val_col].astype(float)
        return etrade

    @staticmethod
    def _preclean_etrade(path):
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
        fidelity = fidelity[fidelity['Last Price'].notnull()]
        fidelity = fidelity[fidelity['Last Price'] != '--']
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
        fidelity = pd.concat([df for df in fidelity.values()], axis=1)
        fidelity.columns = ['rollover', 'roth', 'simple']
        fidelity['fid'] = fidelity.sum(axis=1)
        return fidelity

    @staticmethod
    def _convert_value(s):
        return round(float(s.replace('$', '').replace(',', '')))

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
        return schwab

    def _parse_schwab(self, path):
        has_quotes = False
        with open(path, 'r') as f:
            for line in f:
                if line.startswith('"Symbol'):
                    has_quotes = True
                    break
        data, inds = self._parse_schwab_quoted(path, has_quotes)
        return pd.DataFrame({'schwab': data}, index=inds)
        
    @staticmethod
    def _parse_schwab_quoted(path, has_quotes):
        data = []
        inds = []
        is_header = True
        with open(path, 'r') as f:
            for line in f:
                cash = '"Cash' if has_quotes else 'Cash'
                acct = '"Account' if has_quotes else 'Account'
                if line.startswith(cash) or line.startswith(acct):
                    continue
                if not is_header:
                    col_split = ',"' if has_quotes else ','
                    cols = line.split(col_split)
                    symbol = cols[0].strip('""')
                    dollar = '"$' if has_quotes else '$'
                    try:
                        amt = float(cols[6].strip(dollar))
                    except ValueError:
                        amt = 0
                    except:
                        raise
                    inds.append(symbol)
                    data.append(amt)
                sym = '"Symbol"' if has_quotes else 'Symbol'
                if line.startswith(sym):
                    is_header = False
        return data, inds

    #def _upload_dm(self):
    #    dm = pd.read_csv(
    #        f'{DOWNLOADS}/Dongmei.csv',
    #        index_col=0,
    #        usecols=['Position', 'Market Value']
    #    ).rename(columns={'Market Value': 'dm'})
    #    dm.index = [x.replace(' shares', '') for x in dm.index]
    #    return dm

    def _upload_sims(self):
        print('Uploading simulation data...')
        files = [
            f'Holdings - Damian Satterthwaite-Phillips{x}.csv' for
            x in ['', '(1)', '(2)', '(3)', '(4)']]  # TODO: arbitrary no.
        print(f'Found {len(files)} sim files.')
        dfs = []
        for f in files:
            df = pd.read_csv(
                f'{DOWNLOADS}/{f}', index_col=0, usecols=['Symbol', 'Value'])
            df.Value = df.Value.str.replace(',', '').str[1:].astype(float)
            dfs.append(df)
        out = pd.concat(dfs, axis=1)
        out.columns = [f'sim{i}' for i in range(1, len(files) + 1)]
        return out

    
if __name__ == '__main__':
    HoldingsLoader()._upload_schwab()
