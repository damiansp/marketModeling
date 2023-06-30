import numpy as np
import pandas as pd


class TransactionDeterminer:
    def __init__(
            self, metrics, next_day_distributions, buy_stats, frac_in,
            p_stats0_buy, transact_if):
        self._df = metrics
        self.next_day_distributions = next_day_distributions
        self.buy_stats = buy_stats
        self.frac_in = frac_in
        self.p_stats0_buy = p_stats0_buy
        self.transact_if = transact_if

    @property
    def df(self):
        return self._df

    def compile_data(self):
        self._add_account_indicators()
        self._add_status()
        self._add_scaled_sharpes()
        self._get_status_weights()
        self._get_et_proportions()
        self._get_fid_proportions()
        self._get_tdam_proportions()
        
    def _add_account_indicators(self):
        df_stocks = set(self._df.index)
        bs_stocks = set(self.buy_stats.index)
        print('bs only:', bs_stocks - df_stocks)
        print('df only:', df_stocks - bs_stocks)
        self._df = pd.concat(
            [self._df,
             self.buy_stats[[
                 'inEt', 'inFid', 'in_self_managed', 'currentlyActive']]],
            axis=1)
        
    def _add_status(self):
        SCALED_BOUNDS = 5
        self._df['status'] = self._get_harmonic_mean(
            'RSI', 'fair_value_mult', 'geomean')
        self._df['status_scaled'] = (
            (0.6 * np.tan(3 * (1 - self._df['status']) - 1.5))
            .clip(-SCALED_BOUNDS, SCALED_BOUNDS))

    def _get_harmonic_mean(self, *cols):
        n_cols = len(cols)
        recip_sum = 0
        for col in cols:
            recip_sum += 1 / self._df[col]
        return n_cols / recip_sum

    def _get_status_weights(self):
        self._df['sharpe_adj_status'] = (
            self._df.sharpe_scaled ** self._df.status_scaled)
        self._df['w_sharpe_adj_status'] = (
            self._df.weighted_sharpe_scaled ** self._df.status_scaled)
        self._df['mean_sharpe_adj_status'] = (
            (self._df.sharpe_adj_status + self._df.w_sharpe_adj_status) / 2)

    def _add_scaled_sharpes(self):
        for col in ['sharpe', 'weighted_sharpe']:
            lower = self._df[col].quantile(q=0.02)
            upper = self._df[col].quantile(q=0.98)
            self._df[f'{col}_capped'] = self._df[col].clip(lower, upper)
            min_capped = self._df[f'{col}_capped'].min()
            self._df[f'{col}_scaled'] = (
                self._df[f'{col}_capped'] - min_capped + 1)
        
    def _get_et_proportions(self):
        prop_et = (
            (self._df.sharpe_scaled**4)
            * self._df.w_sharpe_adj_status
            * self._df.inEt)
        prop_sum = prop_et.sum()
        # Originally 0.05, but led to an actual cap closer to 0.17 when
        # renormalized
        capped_et = prop_et.apply(lambda x: min(x, 0.01*prop_sum))  
        self._df['et_norm'] = capped_et / capped_et.sum()
        
    def _get_fid_proportions(self):
        prop_fid = (
            (self._df.weighted_sharpe_scaled**4)
            * self._df.sharpe_adj_status
            * self._df.inFid)
        prop_sum = prop_fid.sum()
        capped_fid = prop_fid.apply(lambda x: min(x, 0.01*prop_sum))
        self._df['fid_norm'] = capped_fid / capped_fid.sum()

    def _get_tdam_proportions(self):
        prop_tdam = (
            (self._df.weighted_sharpe_scaled**4)
            * self._df.mean_sharpe_adj_status
            * self._df.in_self_managed
            * self._df.currentlyActive)
        prop_sum = prop_tdam.sum()
        capped_tdam = prop_tdam.apply(lambda x: min(x, 0.01*prop_sum))
        self._df['tdam_norm'] = capped_tdam / capped_tdam.sum()

    def get_target_amounts(self, account, amount):
        print(f'Getting target amounts for {account}...')
        self._df[f'{account}_target'] = (
            amount * self.frac_in * self._df[f'{account}_norm'])
        self._df[f'{account}_diff'] = (
            self._df[f'{account}_target'] - self._df[account])

    def get_bid_ask_prices(self, account):
        print(f'Getting bid and ask prices for {account}...')
        bid_ask_multiplier = (
            self
            ._df[['direction', 'status_scaled', f'{account}_diff']]
            .apply(lambda row: self._get_bid_ask(row, account), axis=1))
        self._df[f'{account}_bid_ask'] = bid_ask_multiplier * self._df.price
        
    def _get_bid_ask(self, row, account):
        try:
            distr = self._get_status_distribution(row, account)
            q = self._get_bid_ask_quantile_from_status_scaled(
                row.status_scaled, account, row[f'{account}_diff'])
            return distr.quantile(q=q)
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
        distr = self.next_day_distributions.loc[
            self.next_day_distributions[f'{symbol}_trend'] == trend,
            f'{symbol}_{high_low}_mult']
        distr = distr[distr.notnull()]
        return distr

    def _get_bid_ask_quantile_from_status_scaled(self, status, account, diff):
        # Prob of which buy/sell should happen given neutral status (0)
        #P_STATUS0_BUY = {'et': 0.3, 'fid': 0.4, 'tdam': 0.5}[account]
        P_STATUS0_BUY = self.p_stats0_buy[account]
        #             P(buy/sell) at state =
        # p_stat0_buy     0      1      2      3      4     5
        #       0.01   0.01   0.20   0.39   0.57   0.76  0.95
        #       0.02   0.02   0.21   0.39   0.58   0.76  0.95
        #       0.05   0.05   0.23   0.41   0.59   0.77  0.95
        #       0.10   0.10   0.27   0.44   0.61   0.78  0.95
        #       0.20   0.20   0.35   0.50   0.65   0.80  0.95
        #       0.50   0.50   0.59   0.68   0.77   0.86  0.95
        P_STATUS0 = P_STATUS0_BUY if diff >= 0 else 1 - P_STATUS0_BUY
        MIN_P = 0.01  # p(buy | strong sell sig) or vice versa
        MAX_P = 0.95  # p(buy | strong buy sig)  ...
        # Piecewise linear interpolation: y = mx + b: b = P_STATUS_0
        # rise = P_STATUS0 - MIN if negative status (sell signal)
        # rise = MAX - P_STATUS0 if + status (buy signal)
        rise = P_STATUS0 - MIN_P if status <= 0 else MAX_P - P_STATUS0
        m = rise / 5
        q = m*status + P_STATUS0
        return q

    def get_n_shares_to_buy_or_sell(self, account):
        print('Determining ideal number of shares to buy/sell...')
        self._df[f'{account}_nshares'] = (
            self._df[f'{account}_diff'] / self._df[f'{account}_bid_ask'])
        self._df[f'{account}_nshares'].replace(
            [np.inf, -np.inf, np.nan], 0, inplace=True)
        self._df[f'{account}_nshares'] = (
            self._df[f'{account}_nshares'].round().astype(int))

    def list_transactions(self, account, invested_amt, daily_transaction_amt):
        ''' Determine the specific stocks and no. shares to buy/sell each day
        Args:
        - account (str): account name
        - invested_amt (float): total $ that should be invested as of today for
          <account>
        - daily_transaction_amt (float): ideal amt to buy/sell each day (may be
          more if % funds invested changes).
        '''
        print('Getting transactions...')
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
                self._handle_transactions(account, err, 'sell')
            else:
                print('Just buying today\n')
                self._handle_transactions(account, err, 'buy')
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

    def _sort_by_transaction_order(self, transaction_type):
        if transaction_type == 'sell':
            ascending = [True, True]
        elif transaction_type == 'buy':
            ascending = [False, False]
        else:
            raise ValueError('transaction_type must be "buy" or "sell"')
        self._df.sort_values(
            ['up_down', 'status_scaled'], ascending=ascending, inplace=True)

    def _handle_transactions(self, account, err, transaction_type, curr_opp):
        self._sort_by_transaction_order(transaction_type)
        cum = self._df.exact_amt.cumsum()
        for i, (trans_total, symbol, shares, bid_ask, status) in enumerate(
                zip(
                    cum,
                    self._df.index,
                    self._df[f'{account}_nshares'],
                    self._df[f'{account}_bid_ask'],
                    self._df.status_scaled)):
            if shares == 0:
                continue
            thresh = self.transact_if[account][curr_opp]
            if abs(status) < thresh:
                if transaction_type == 'buy':
                    diff = thresh - abs(status)
                    shares = int(round(shares * (2 ** -diff)))
            if transaction_type == 'sell' and status > -thresh:
                shares = 0
            if ((transaction_type == 'buy' and shares > 0)
                or (transaction_type == 'sell' and shares < 0)):
                print(
                    f'{transaction_type.title():4s} {shares:+4d} shares of '
                    f'{symbol:5s} at ${bid_ask:7,.2f} (Total: '
                    f'${abs(shares) * bid_ask:9,.2f}) '
                    f'Status: {status:.3f}')
            if abs(trans_total) >= abs(err):
                return
