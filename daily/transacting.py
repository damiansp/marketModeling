import numpy as np


class TransactionDeterminer:
    def __init__(
            self, metrics, next_day_distributions, frac_in, p_stats0_buy,
            params):
        self._df = metrics
        self.next_day_distributions = next_day_distributions
        self.frac_in = frac_in
        self.p_stats0_buy = p_stats0_buy
        self.params = params

    @property
    def df(self):
        return self._df

    def compile_data(self):
        self._add_status()
        self._add_scaled_sharpes()
        self._get_status_weights()
        sims = [k for k in self.params.keys() if k.startswith('sim')]
        for portfolio in ['et', 'fid', 'schwab', 'dm'] + sims:
            self._get_proportions(portfolio)

    def _add_status(self):
        for portfolio, params in self.params.items():
            weights = params['status_weights']
            self._df[f'{portfolio}_status'] = self._get_weighted_harmonic_mean(
                weights, 'RSI', 'fair_value_mult', 'geomean')
            scaler_scalar = params['scaler']
            self._df[f'{portfolio}_status_scaled'] = (
                (scaler_scalar
                 * np.tan(3 * (1 - self._df[f'{portfolio}_status']) - 1.5)))

    def _get_weighted_harmonic_mean(self, weights, *cols):
        s_w = sum(weights)
        denom = 0
        for w, col in zip(weights, cols):
            denom += (w / self._df[col])
        return s_w / denom

    def _add_scaled_sharpes(self):
        lower = self._df.sharpe.quantile(q=0.02)
        upper = self._df.sharpe.quantile(q=0.98)
        self._df['sharpe_capped'] = self._df.sharpe.clip(lower, upper)
        min_capped = self._df.sharpe_capped.min()
        self._df['sharpe_scaled'] = (self._df.sharpe_capped - min_capped + 1)

    def _get_status_weights(self):
        for portfolio in self.params:
            self._df[f'{portfolio}_sharpe_adj_status'] = (
                self._df.sharpe_scaled
                ** self._df[f'{portfolio}_status_scaled'])

    def _get_proportions(self, portfolio):
        print(f'Getting proportions for {portfolio}...')
        params = self.params[portfolio]
        in_portfolio = {
            'et': 'inEt',
            'fid': 'inFid',
            'schwab': 'in_self_managed',
            'dm': 'inFid'
        }.get(portfolio, 'in_self_managed')
        EXP = params['sharpe_scaled_exp']
        try:
            prop = (
                (self._df.sharpe_scaled ** EXP)
                * self._df[f'{portfolio}_sharpe_adj_status']
                * self._df[in_portfolio]
                * self._df.currentlyActive)
        except:
            self._df.to_csv('~/Desktop/test.csv', index=False)
            print('Values:')
            print(
                self._df[
                    ['sharpe_scaled', f'{portfolio}_sharpe_adj_status',
                     in_portfolio, 'currentlyActive']])
            raise
        max_prop = params['max_prop_per_stock']
        self._df[f'{portfolio}_norm'] = self._rescale_props(prop, max_prop)
        
    @staticmethod
    def _rescale_props(prop, max_prop=0.05):
        prop_sum = prop.sum()
        prop  = prop / prop_sum
        i = 1
        print('Rescaling proportions to limit to max')
        while prop.max() > max_prop:
            prop = prop.clip(upper=0.99*max_prop)
            prop = prop / prop.sum()
            i += 1
            if (i > 100):
                break
        print(f'Actual max prop before clipping: {prop.max():.4f}')
        return prop.clip(upper=max_prop)

    def get_target_amounts(self, account, amount):
        print(f'Getting target amounts for {account}...')
        self._df[f'{account}_target'] = (
            amount * self.frac_in * self._df[f'{account}_norm'])
        self._df[f'{account}_diff'] = (
            self._df[f'{account}_target'] - self._df[account])

    def get_bid_ask_prices(self, account):
        print(f'Getting bid and ask prices for {account}...')
        buy_level = self.params[account]['buy_level']
        sell_level = self.params[account]['sell_level']
        bid_ask_multiplier = (
            self
            ._df[['direction', f'{account}_status_scaled', f'{account}_diff']]
            .apply(
                lambda row: self._get_bid_ask(
                    row, account, buy_level, sell_level), axis=1))
        self._df[f'{account}_bid_ask'] = bid_ask_multiplier * self._df.price

    def _get_bid_ask(self, row, account, buy_level, sell_level):
        try:
            distr = self._get_status_distribution(row, account)
            if distr is None:
                return np.nan
            q = self._get_bid_ask_quantile_from_status_scaled(
                row[f'{account}_status_scaled'],
                account,
                buy_level,
                sell_level,
                row[f'{account}_diff'])
            # make sure q on [0.01, 0.99]
            if q < 0.01:
                q = 0.01
            elif q > 0.99:
                q = 0.99
            multiplier = distr.quantile(q=q)
            return multiplier
        except:
            print('row:')
            print(row)
            print('acct:', account)
            distr = self._get_status_distribution(row, account)
            print('distr:')
            print(distr)
            print(
                'Notice this error sometimes due to bad data from Yahoo. '
                'Just retry.')
            raise

    def _get_status_distribution(self, row, account):
        symbol = row.name
        trend = row.direction
        high_low = 'low' if row[f'{account}_diff'] > 0 else 'high'
        try:
            distr = self.next_day_distributions.loc[
                self.next_day_distributions[f'{symbol}_trend'] == trend,
                f'{symbol}_{high_low}_mult']
            distr = distr[distr.notnull()]
        except KeyError:
            distr = None
        return distr

    def _get_bid_ask_quantile_from_status_scaled(
            self, status, account, buy_level, sell_level, diff):
        # Prob of which buy/sell should happen given neutral status (0)
        #P_STATUS0_BUY = {'et': 0.3, 'fid': 0.4, 'schwab': 0.5}[account]
        P_STATUS0_BUY = self.p_stats0_buy[account]
        #             P(buy/sell) at state =
        # p_stat0_buy     0      1      2      3      4     5
        #       0.01   0.01   0.20   0.39   0.57   0.76  0.95
        #       0.02   0.02   0.21   0.39   0.58   0.76  0.95
        #       0.05   0.05   0.23   0.41   0.59   0.77  0.95
        #       0.10   0.10   0.27   0.44   0.61   0.78  0.95
        #       0.20   0.20   0.35   0.50   0.65   0.80  0.95
        #       0.50   0.50   0.59   0.68   0.77   0.86  0.95
        P_STATUS0 = (
            P_STATUS0_BUY['buy'] if diff >= 0 else 1 - P_STATUS0_BUY['sell'])
        level = buy_level if diff >= 0 else sell_level
        MIN_P = 0.03  # p(buy | strong sell sig) or vice versa
        MAX_P = 0.97  # p(buy | strong buy sig)  ...
        # Piecewise linear interpolation: y = mx + b: b = P_STATUS_0
        # rise = P_STATUS0 - MIN if negative status (sell signal)
        # rise = MAX - P_STATUS0 if + status (buy signal)
        rise = P_STATUS0 - MIN_P if status <= 0 else MAX_P - P_STATUS0
        m = rise / level  # level: status as which prob reaches 1
        q = m*status + P_STATUS0
        q = min(max(q, MIN_P), MAX_P)
        return q

    def get_n_shares_to_buy_or_sell(self, account):
        print('Determining ideal number of shares to buy/sell...')
        self._df[f'{account}_nshares'] = (
            self._df[f'{account}_diff'] / self._df[f'{account}_bid_ask'])
        self._df[f'{account}_nshares'].replace(
            [np.inf, -np.inf, np.nan], 0, inplace=True)
        self._df[f'{account}_nshares'] = (
            self._df[f'{account}_nshares'].round().astype(int))

    def list_transactions(
            self, account, invested_amt, daily_transaction_amt, total_amt):
        '''Determine the specific stocks and no. shares to buy/sell each day
        Args:
        - account (str): account name
        - invested_amt (float): total $ that should be invested as of today for
          <account>
        - daily_transaction_amt (float): ideal amt to buy/sell each day (may be
          more if % funds invested changes).
        - total_amt (float): total funds in <account>
        '''
        print('Getting transactions...')
        print(f'Total amount: {total_amt:,}')
        self._df['up_down'] = 1 * (self._df[f'{account}_diff'] > 0)
        self._df['exact_amt'] = (
            self._df[f'{account}_nshares'] * self._df[f'{account}_bid_ask'])
        err = self._df[f'{account}_diff'].sum()  # optimal amount to buy/sell
        print(
            f'Ideal invested amt: ${invested_amt:,.2f}\n'
            f'Currently off by: ${err:,.2f}\n'
            f'Ideal transaction amt: ${daily_transaction_amt:,.2f}')
        if abs(err) >= daily_transaction_amt:
            # all buys or all sells
            if err < 0:
                print('Just selling today\n')
                self._handle_transactions(account, err, 'sell', 'curr')
            else:
                print('Just buying today\n')
                self._handle_transactions(account, err, 'buy', 'curr')
        else:
            print('Buying and selling today\n')
            primary = abs(err)
            remainder = daily_transaction_amt - primary
            primary += remainder / 2
            secondary = remainder / 2
            if err <= 0:
                # primary is sell
                self._handle_transactions(account, primary, 'sell', 'curr')
                print()
                self._handle_transactions(account, secondary, 'buy', 'opp')
            else:
                # primary is buy
                self._handle_transactions(account, primary, 'buy', 'curr')
                print()
                self._handle_transactions(account, secondary, 'sell', 'opp')

    def _handle_transactions(self, account, err, transaction_type, curr_opp):
        self._sort_by_transaction_order(transaction_type, account)
        cum = self._df.exact_amt.cumsum()
        for i, (trans_total, symbol, shares, bid_ask, status) in enumerate(
                zip(
                    cum,
                    self._df.index,
                    self._df[f'{account}_nshares'],
                    self._df[f'{account}_bid_ask'],
                    self._df[f'{account}_status_scaled'])):
            if shares == 0:
                continue
            if ((transaction_type == 'buy' and shares > 0)
                or (transaction_type == 'sell' and shares < 0)):
                print(
                    f'{transaction_type.title():4s} {shares:+4d} shares '
                    f'of {symbol:5s} at ${bid_ask:7,.2f} (Total: '
                    f'${abs(shares) * bid_ask:9,.2f}) '
                    f'Status: {status:.3f}')
            if abs(trans_total) >= abs(err):
                return

    def _sort_by_transaction_order(self, transaction_type, account):
        if transaction_type == 'sell':
            ascending = [True, True]
        elif transaction_type == 'buy':
            ascending = [False, False]
        else:
            raise ValueError('transaction_type must be "buy" or "sell"')
        self._df.sort_values(
            ['up_down', f'{account}_status_scaled'],
            ascending=ascending,
            inplace=True)

        
