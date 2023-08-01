from datetime import datetime, timedelta

import pandas as pd


def get_adjusted_date():
    'Treat weekend dates as preceding Friday'
    NOW = datetime.now()
    #today = NOW.date()
    weekday = NOW.weekday()
    if weekday == 5:    # Sat
        return NOW - timedelta(1)
    elif weekday == 6:  # Sun
        return NOW - timedelta(2)
    return NOW


DOWNLOADS = '/Users/damiansp/Downloads'
now = get_adjusted_date()
TODAY = now.date()
TOMORROW = (now + timedelta(1)).date()


class HoldingsAppender:
    def __init__(self, stock_metrics):
        if 'stock' in list(stock_metrics):
            stock_metrics.set_index('stock', inplace=True)
        #print('Stock metrics:')
        #print(stock_metrics.head())
        #sys.exit()
        self.stock_metrics = stock_metrics
        self.SYMBOLS = list(set(stock_metrics.index.tolist()))
        self.binder = pd.DataFrame(index=self.SYMBOLS)

    def append_holdings(self):
        etrade, fidelity, td_ameritrade = self._upload_files()
        out = pd.concat([self.binder, etrade, td_ameritrade], axis=1)
        for v in fidelity.values():
            out = pd.concat([out, v], axis=1)
        out = out.astype(float).fillna(0).round().astype(int)
        out_cols = ['et', 'tdam', 'rollover', 'roth', 'simple']
        out.columns = out_cols
        for col in out_cols + ['fid']:
            if col in list(self.stock_metrics):
                self.stock_metrics.drop(columns=[col], inplace=True)
        out['fid'] = out.rollover + out.roth + out.simple
        metrics = pd.concat([self.stock_metrics, out], axis=1)
        metrics['Owned'] = metrics[out_cols].sum(axis=1)
        return metrics

    def _upload_files(self):
        etrade = self._upload_etrade()
        fidelity = self._upload_fidelity()
        td_ameritrade = self._upload_tdameritrade()
        return etrade, fidelity, td_ameritrade
        
    def _upload_etrade(self):
        print('Uploading E*Trade data...')
        filename = 'Positions.csv'
        etrade = (
            pd
            .read_csv(
                f'{DOWNLOADS}/{filename}', index_col=0, skiprows=1
            )[['Market Value']]
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

    def _upload_fidelity(self):
        print('Uploading Fidelity data...')
        today = datetime.strftime(TODAY, '%b-%d-%Y')
        filename = f'Portfolio_Positions_{today}.csv'
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

    def _upload_tdameritrade(self):
        print('Uploading TD Ameritrade data...')
        filename = (
            f'Positions_damiansp_Stocks_{str(TODAY).replace("-", "_")}.xls')
        try:
            tdam = self._parse_tdam_file(filename)
        except FileNotFoundError:
            filename = (
                f'Positions_damiansp_Stocks_{str(TOMORROW).replace("-", "_")}'
                f'.xls')
            tdam = self._parse_tdam_file(filename)
        if not self._symbols_are_valid(tdam):
            raise ValueError('Unexpected symbol in TD Ameritrade file')
        return tdam

    @staticmethod
    def _parse_tdam_file(filename):
        inds = []
        data = []
        is_header = True
        print('reading from', filename)
        with open(f'{DOWNLOADS}/{filename}', 'r') as f:
            for line in f:
                if not is_header:
                    if line.startswith('"'):
                        try:
                            columns = line.split('\t')
                            symbol = columns[0][1:-1]
                            value = float(columns[5][1:-1].replace(',', ''))
                            inds.append(symbol)
                            data.append(value)
                        except IndexError:
                            print('Cannot parse:', line)
                if line.startswith('Symbol'):
                    is_header = False
        return pd.DataFrame({'TD': data}, index=inds)
            
    @staticmethod
    def _convert_value(s):
        return round(float(s.replace('$', '').replace(',', '')))
        
    def _symbols_are_valid(self, df):
        for symbol in df.index.unique():
            if symbol not in self.SYMBOLS:
                print(f'{symbol} not in SYMBOLS')
                return False
        return True
