'''
A simple model with the following constraints:
- As the value of a stock increases (relative to an arbitrary trend), the 
  percentage of funds invested must decrease monotonically, and vice versa 
  (larger percents invested when the value is low).  In short, this is a 
  "buy low, sell high" stategy.
- Given a user-selected number of segments, the model will search to 
  optimize the location of the cutpoints and the amount to buy or sell at 
  those cutpoints.

For example, with a single cutpoint, the model will find the single 
cutpoint, (quantile relative to the specified trend), and the amount to 
buy when the stock value falls below that cutpoint, and the amount to sell
when it rises above it.  So, for instance, if the cutpoint were the median,
a portion of the stock would be sold when the value drops below the median,
etc.  However, the user specifies only the number of cutpoints, and the 
model optimizes the quantile, and the percentages to buy or sell.

Caveats:
- Because the search space is monotonic, the search is optimal *in the 
  limit*, and should approach it asymptotically as search iterations 
  increase
- The parameters are unlikely to be temporally stable. As time passes, and
  more data accumulates, the optimal values may change.  The longer the 
  stock history, the more stable the values will be, and less likely to 
  change as a result of recent movements
- "Optimal" is relative to the backtest, and is no guarantee that the 
  values will be optimal going forward.
- The greater the number of cutpoints selected (and the shorter the stock
  history), the greater the likelihood of overfitting the parameters to the
  historical data, and the lesser the likelihood of them generalizing well
  going forward.
'''
#import sys

import matplotlib.pyplot as plt
import numpy as np
#import pandas as pd

#sys.path.append('..')
#from util.util import sharpe_from_daily, silence_pandas


FIGSIZE = (16, 8)
TREND_METHODS = ['ma', 'ew_ma', 'rel_minmax', 'ew_rel_minmax']
MIN_TIME_PARAM = 10

    
class QPercent:
    def __init__(
            self, data, n_segments, max_time_param=None,
            best_param_set=None, best_returns=None, sd=0.5,
            p_change_time_param=None, ma_factor=0.1, verbose=False):
        self._data = data
        self.n = data.shape[0]
        self.n_segments = n_segments
        self.max_time_param = (max_time_param if max_time_param is not None
                               else self.n // 5)
        self.best_param_set = (
            best_param_set if best_param_set is not None
            else {method: None for method in TREND_METHODS})
        self.best_returns = (
            best_returns if best_returns is not None
            else {method: None for method in TREND_METHODS})
        for method, val in self.best_returns.items():
            try:
                if val is None or np.isnan(val):
                    self.best_returns[method] = 0
            except:
                print('val:', val, '\nbest_returns:', self.best_returns[method])
        self.sd = sd
        self.p_change_time_param = p_change_time_param
        self.ma_factor = ma_factor
        self.do_nothing_returns = None
        self.start_date = self._data.iloc[self.max_time_param]['Date']
        self.verbose = verbose

    @property
    def data(self):
        return self._data
    
    @staticmethod
    def _normalize(timeseries):
        start = timeseries.tolist()[0]
        return timeseries / start

    def get_ma(self, data, time_param, ew=False):
        '''
        Calculate the moving average (of the LogValue of the stock)
        Args:
        - ew (bool): use exponential weighting?
        - time_param:
          - ew=True: (real) the halflife of the exponential weights (no. 
　　　　　　　of  time periods it takes for weight to drop from 1 to 0.5)
          - ew=False: (int) the ma window, or number of days to average 
　　　　　　　over
        '''
        if ew:
            data['Trend'] = (
                data.LogValue.ewm(halflife=time_param, ignore_na=True)
                .mean())
        else:
            data['Trend'] = (data.loc[:, 'LogValue']
                             .rolling(window=int(time_param))
                             .mean())
        return data

    def get_rel_minmax(self, data, time_param, ew=False):
        data['RollMax'] = (
            data.LogValue.rolling(window=time_param).max())
        data['RollMin'] = (
            data.LogValue.rolling(window=time_param).min())
        if ew:
            data['Trend'] = (
                (data.LogValue - data.RollMin)
                / (data.RollMax.ewm(halflife=time_param).mean()
                   - data.RollMin.ewm(halflife=time_param).mean()))
        else:
            data['Trend'] = (
                (data.LogValue - data.RollMin)
                / (data.RollMax - data.RollMin))
        return data
        
    def get_deviate_quantiles(self, data, trend_method):
        '''
        Get deviates (as quantiles) relative to trend
        '''
        ###
        if 'minmax' in trend_method:
            data['Deviates'] = data.Trend
        else:
            ###
            data['Deviates'] = data.LogValue - data.Trend
        n_notnan = len(data.Deviates[~np.isnan(data.Deviates)])
        qs = list(np.linspace(1, 0, n_notnan))
        quantiles  = [np.nan] * self.n
        for i in data.sort_values('Deviates').index:
            if np.isnan(data.loc[i, 'Deviates']):
                continue
            else:
                quantiles[i] = qs.pop()
        return quantiles

    def run_sims(self, n_rand=1, n_adj=1):
        plt.figure(figsize=FIGSIZE)
        i = 1
        for method in TREND_METHODS:
            print(f'\n\nBeginning {method} method...')
            print('-' * 50)
            data = self.data.copy()
            restr = data.loc[data.Date >= self.start_date, :]
            self.do_nothing_returns = (
                self._normalize(restr.Value).tolist()[-1])
            print('Do nothing returns:', self.do_nothing_returns)
            if self.do_nothing_returns > self.best_returns[method]:
                self.best_returns[method] = self.do_nothing_returns
            #plt.figure(figsize=FIGSIZE)
            plt.subplot(2, 2, i)
            plt.plot(restr.Date, self._normalize(restr.Value))
            if self.best_param_set[method] is None:
                self.best_returns[method] = self.do_nothing_returns
                self.best_param_set[method] = self._get_default_params(method)
            self._refit_best_params(method)
            self._run_random_search(n_rand, method)
            self._run_adjusted_search(n_adj, method)
            plt.title(method)
            plt.yscale('log')
            i += 1
        plt.show()
        
    def _get_default_params(self, method):
        params = {'trend': method,
                  'time_param': self.max_time_param // 2,
                  'q_params': self.default_q_params}
        return params

    @property
    def default_q_params(self, lam=0.999):
        q_params = {}
        amt = 1.
        cuts = np.linspace(0, 1, self.n_segments + 1)
        for i in range(len(cuts) - 1):
            t = (cuts[i], cuts[i + 1])
            q_params[t] = amt
            amt *= lam
        return q_params
        
    def _refit_best_params(self, method):
        print('Refitting current best...')
        res = self._backtest(self.best_param_set[method])
        returns = res.total.tolist()[-1]
        if np.isnan(returns) or returns < self.do_nothing_returns:
            self.best_returns[method] = self.do_nothing_returns
            current_pct = 1
        else:
            self.best_returns[method] = returns
            print(f'Current best {method} returns:',
                  self.best_returns[method])
            current_pct = res.pct_invested.tolist()[-1]
        print('Fraction Invested:', current_pct)
        self.best_param_set[method]['pct'] = current_pct

    def _run_random_search(self, n, method):
        if n == 0:
            return
        print('Beginning random search...')
        for i in range(n):
            print('Round:', i + 1, end='\r')
            params = self._get_rand_params(method)
            res = self._backtest(params)
            # add to existing plot call in run_sims()
            plt.plot(res.Date, res.total, alpha=0.3)
            returns = res.total.tolist()[-1]
            if np.isnan(returns):
                continue
            elif returns > self.best_returns[method]:
                print('New best:', returns)
                self.p_print(params)
                self.best_param_set[method] = params
                self.best_returns[method] = returns
                current_pct = res.pct_invested.to_list()[-1]
                print('Invested:', current_pct)
                self.best_param_set[method]['pct'] = current_pct
        
        print()

    def _run_adjusted_search(self, n, method):
        print('Beginning adjusted search...')
        for i in range(n):
            print('Round:', i + 1, end='\r')
            params = self._adjust_params(method)
            res = self._backtest(params)
            # add to existing plot call in run_sims()
            plt.plot(res.Date, res.total, alpha=0.3)
            returns = res.total.tolist()[-1]
            if np.isnan(returns):
                continue
            elif returns > self.best_returns[method]:
                print('New best:', returns)
                self.p_print(params)
                self.best_returns[method] = returns
                self.best_param_set[method] = params
                current_pct = res.pct_invested.to_list()[-1]
                print('Invested:', current_pct)
                self.best_param_set[method]['pct'] = current_pct
        print()

    def _get_rand_params(self, method, time_param_range=None):
        if time_param_range is None:
            time_param_range = [MIN_TIME_PARAM, self.max_time_param]
        cuts = [0] + sorted(np.random.uniform(size=self.n_segments - 1))
        cuts.append(1)
        amts = sorted(np.random.uniform(size=self.n_segments))[::-1]
        ranges = [(cuts[i], cuts[i + 1]) for i in range(self.n_segments)]
        q_params = {rg: amt for (rg, amt) in zip(ranges, amts)}
        time_param = int(round(np.random.uniform(*time_param_range)))
        trend_method = method
        params = {'time_param': time_param,
                  'trend': trend_method,
                  'q_params': q_params}
        return params

    def _adjust_params(self, method):
        params = self.best_param_set[method].copy()
        n_params = len(params['q_params'])
        p_change_time_param = (self.p_change_time_prarm
                               if self.p_change_time_param is not None
                               else 1 / (n_params + 1))
        adj_time_param = np.random.choice(
            [True, False],
            p=(p_change_time_param, 1 - p_change_time_param))
        if adj_time_param:
            time_param = params['time_param']
            time_param += np.random.normal(scale=self.sd * self.ma_factor)
            time_param = int(
                round(
                    max(min(time_param, self.max_time_param),
                        MIN_TIME_PARAM)))
            params['time_param'] = time_param
            return params
        adj_ranges = np.random.choice([True, False])
        ranges = params['q_params'].keys()
        if adj_ranges:
            amts = params['q_params'].values()
            cuts = sorted([0] + [r[1] for r in ranges])
            i = np.random.choice(range(1, n_params))
            cuts[i] += np.random.normal(scale=self.sd)
            if cuts[i] <= cuts[i - 1] or cuts[i] >= cuts[i + 1]:
                cuts[i] = np.random.uniform(cuts[i - 1], cuts[i + 1])
            ranges = [(cuts[i], cuts[i + 1]) for i in range(n_params)]
        else:
            amts = [0] + sorted(params['q_params'].values()) + [1]
            i = np.random.choice(range(1, n_params + 1))
            amts[i] += np.random.normal(scale=self.sd)
            if amts[i] <= amts[i - 1] or amts[i] >= amts[i + 1]:
                amts[i] = np.random.uniform(amts[i -1], amts[i + 1])
            amts = amts[1:-1]
        q_params = {rg: amt
                    for (rg, amt) in zip(ranges, sorted(list(amts))[::-1])}
        params['q_params'] = q_params
        return params

    def p_print(self, params):
        if not self.verbose:
            return
        opn = '{'
        #cls = '}'
        print(f"{opn}'time_param': {params['time_param']},"
              f"\n 'trend': {params['trend']},"
              f"\n 'q_params': {opn}")
        for rg, val in params['q_params'].items():
            print(f'    ({rg[0]:.5f}, {rg[1]:.5f}): {val:.5f},')
        print('}}')

    def _backtest(self, params):
        data = self.data.copy()
        data['pct_invested'] = self._get_pct_invested(params, data)
        data = data[~np.isnan(data.pct_invested)]
        data['invested'] = data.pct_invested # init
        data = data.loc[data.Date >= self.start_date, :]
        data['reserve'] = 1 - data.invested
        data['total'] = 1
        data.index = range(data.shape[0])
        for i in data.index[2:]:
            no_trade = (data.loc[i, 'pct_invested']
                        == data.loc[i - 2, 'pct_invested'])
            if no_trade:
                # just adjust by day-to-day change
                data.loc[i, 'invested'] = (data.loc[i - 1, 'invested']
                                           * data.loc[i, 'DayToDayChange'])
                data.loc[i, 'reserve'] = data.loc[i - 1, 'reserve']
            else:
                target = (data.loc[i - 1, 'pct_invested']
                          * data.loc[i - 1, 'total'])
                move_amt = target - data.loc[i - 1, 'invested']
                amt_day_start = (data.loc[i - 1, 'invested']
                                 * data.loc[i, 'OvernightChange']
                                 + move_amt)
                data.loc[i, 'invested'] = (amt_day_start
                                           * data.loc[i, 'IntradayChange'])
                data.loc[i, 'reserve'] = (data.loc[i - 1, 'reserve']
                                          - move_amt)
            data.loc[i, 'total'] = (data.loc[i, 'reserve']
                                    + data.loc[i, 'invested'])
        return data
                
    def _get_pct_invested(self, params, data):
        time_param = params['time_param']
        trend_method = params['trend']
        ew = False
        if 'ew_' in trend_method:
            ew = True
            trend_method = trend_method.replace('ew_', '')
        q_params = params['q_params']
        # Calculate trend with currrent settings
        data = {
            'ma': self.get_ma,
            'rel_minmax': self.get_rel_minmax
        }[trend_method](data, time_param, ew)
        #qs = self.get_deviate_quantiles(data)
        qs = self.get_deviate_quantiles(data, trend_method) ###
        pct_invested = [q_params.get(self._get_qrange(q, q_params), np.nan)
                        for q in qs]
        return pct_invested

    @staticmethod
    def _get_qrange(val, params):
        if np.isnan(val):
            return np.nan
        try:
            for qrange in params.keys():
                low, high = qrange
                if high == 1:
                    high = 1.1
                if low <= val < high:
                    return qrange
        except ValueError:
            print('in get qrange:', params) 

    def plot_best(self, method, stock=False):
        params = self.best_param_set[method]
        time_param = params['time_param']
        trend_method = params['trend']
        ew = False
        if 'ew_' in trend_method:
            ew = True
            trend_method = trend_method.replace('ew_', '')
        q_params = params['q_params']
        # Calculate trend with currrent settings
        data = {
            'ma': self.get_ma,
            'rel_minmax': self.get_rel_minmax
        }[trend_method](self.data.copy(), time_param, ew)
        qs = self.get_deviate_quantiles(data, trend_method)
        pct_invested = [q_params.get(self._get_qrange(q, q_params), np.nan)
                        for q in qs]
        if stock:
            plt.figure(figsize=[12, 4])
            plt.plot(data.LogValue)
        plt.figure(figsize=[12, 4])
        plt.plot(qs, label='deviate quantile')
        pct_in = pct_invested[-1]
        plt.plot(pct_invested, label=f'pct in ({pct_in:.5f})')
        for k in q_params.keys():
            plt.axhline(y=k[0], color='k', alpha=0.1)
        plt.legend()
        plt.title(method)
        plt.show()
        return pct_in
        


# Test
if __name__ == '__main__':
    from util.stock_loader import Loader
    
    loader = Loader('TSLA', '2005-01-01')
    loader.difference()
    data = loader.data
    qp_mod = QPercent(data, 2)
    qp_mod.get_ma(28)
    qp_mod.get_deviate_quantiles()
    print(qp_mod.data[['LogValue', 'Trend', 'Deviates', 'Quantiles']])
