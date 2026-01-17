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
        dm = self._upload_dm()
        sims = self._upload_sims()
        ###
        for name, df in zip(
                ('et', 'f', 'sch', 'dm', 'sim'),
                (etrade, fidelity, schwab, dm, sims)):
            print(name)
            if len(df.columns) != len(set(df.columns)):
                print('dupe cols')
                df.to_csv(f'~/Desktop/{name}_dupes.csv')
                exit()
            if len(df.index) != len(set(df.index)):
                print('dupe rows')
                df.to_csv(f'~/Desktop/{name}_dupes.csv')
                exit()
        ###
        out = pd.concat(
            [etrade, fidelity, schwab, dm, sims], axis=1
        ).sort_index()
        out = out.astype(float).fillna(0)
        out['owned'] = out.et + out.fid + out.schwab
        out = out.round().astype(int)
        return out

    def _upload_dm(self):
        df = self._upload_etrade(1)
        df.columns = ['dm']
        return df

    def _upload_etrade(self, pos=None):
        filename = (
            f'Positions({pos}).csv' if pos is not None else 'Positions.csv')
        path = f'{DOWNLOADS}/{filename}'
        self._preclean_etrade(path)
        print('Uploading E*Trade data...')
        val_col = 'dm' if pos is not None else 'et'
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
        self._clean_hanging_comma(filename)
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
    def _clean_hanging_comma(filename):
        with open(f'{DOWNLOADS}/{filename}', 'r') as fi:
            data_in = fi.readlines()
            with open(f'{DOWNLOADS}/{filename}', 'w') as fo:
                for line in data_in:
                    line = line.strip(',\n') + '\n'
                    fo.write(line)
            
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
        path_start = 'PCRA'
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
        value_idx = -1
        with open(path, 'r') as f:
            for line in f:
                cash = '"Cash' if has_quotes else 'Cash'
                acct = '"Account' if has_quotes else 'Account'
                if line.startswith(cash) or line.startswith(acct):
                    continue
                if not is_header:
                    col_split = ',"' if has_quotes else ','
                    cols = [x.strip('"') for x in line.split(col_split)]
                    #print(cols)
                    symbol = cols[0].strip('""')
                    dollar = '"$' if has_quotes else '$'
                    try:
                        amt = float(cols[value_idx].strip(dollar))
                    except ValueError:
                        amt = 0
                    except:
                        raise
                    print(symbol, amt)
                    inds.append(symbol)
                    data.append(amt)
                sym = '"Symbol"' if has_quotes else 'Symbol'
                if line.startswith(sym):
                    is_header = False
                    header_cols = line.split(',')
                    for i, col in enumerate(header_cols):
                        if 'Mkt' in col:
                            value_idx = i
                            break
                    print('val idx:', value_idx)
        return data, inds

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
            print(df.head(10))
            dfs.append(df)
        out = pd.concat(dfs, axis=1)
        out.columns = [f'sim{i}' for i in range(1, len(files) + 1)]
        return out

    
if __name__ == '__main__':
    sch = HoldingsLoader()._upload_schwab()
    print(sch)
