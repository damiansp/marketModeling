import numpy as np
import pandas as pd


class TransactionDeterminer:
    def __init__(self, metrics, next_day_distributions, buy_stats, frac_in):
        self._df = metrics
        self.next_day_distributions = next_day_distributions
        print(next_day_distributions.head())
        self.buy_stats = buy_stats.set_index('stock')
        self.frac_in = frac_in

    def compile_data(self):
        self._add_account_indicators()
        self._add_status()
        self._add_scaled_sharpes()
        self._get_status_weights()
        self._get_et_proportions()
        self._get_fid_proportions()
        self._get_tdam_proportions()
        
    def _add_account_indicators(self):
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
        capped_et = prop_et.apply(lambda x: min(x, 0.05*prop_sum))
        self._df['et_norm'] = capped_et / capped_et.sum()
        
    def _get_fid_proportions(self):
        prop_fid = (
            (self._df.weighted_sharpe_scaled**4)
            * self._df.sharpe_adj_status
            * self._df.inFid)
        prop_sum = prop_fid.sum()
        capped_fid = prop_fid.apply(lambda x: min(x, 0.05*prop_sum))
        self._df['fid_norm'] = capped_fid / capped_fid.sum()

    def _get_tdam_proportions(self):
        prop_tdam = (
            (self._df.weighted_sharpe_scaled**4)
            * self._df.mean_sharpe_adj_status
            * self._df.in_self_managed
            * self._df.currentlyActive)
        prop_sum = prop_tdam.sum()
        capped_tdam = prop_tdam.apply(lambda x: min(x, 0.05*prop_sum))
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
        distr = self._get_status_distribution(row, account)
        q = self._get_bid_ask_quantile_from_status_scaled(
            row.status_scaled, account, row[f'{account}_diff'])
        return distr.quantile(q=q)

    def _get_status_distribution(self, row, account):
        symbol = row.name
        trend = row.direction
        status = row.status_scaled
        high_low = 'low' if row[f'{account}_diff'] > 0 else 'high'
        distr = self.next_day_distributions.loc[
            self.next_day_distributions[f'{symbol}_trend'] == trend,
            f'{symbol}_{high_low}_mult']
        distr = distr[distr.notnull()]
        return distr

    @staticmethod
    def _get_bid_ask_quantile_from_status_scaled(status, account, diff):
        # Prob of which buy/sell should happen given neutral status (0)
        P_STATUS0_BUY = {'et': 0.3, 'fid': 0.4, 'tdam': 0.5}[account]
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
        self._df[f'{account}_nshares'] = (
            self._df[f'{account}_diff'] / self._df[f'{account}_bid_ask']
        ).round().astype(int)
        self._df.to_csv('/tmp/test.csv')

    def list_transactions(self, account, amount):
        pass
