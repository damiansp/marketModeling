import pandas as pd


DECAY = 0.8


class StockHoldingsUpdater:
    def __init__(
            self, current_stocks, current_best_stocks, buy_stats):
        print('Initializing StockHoldingsUpdater:\ncurrent_stocks:')
        print(current_stocks)
        print('\ncurrent best stocks:')
        print(current_best_stocks)
        print('\nbuy_stats:')
        print(buy_stats)
        self.current_stocks = current_stocks
        self.current_best_stocks = current_best_stocks
        self.buy_stats = buy_stats
        self.best = []
        self.old_best = self.current_stocks['best_weighted']

    def update_current_stocks(self):
        self._update_lingerers()
        self._insert_current_best_stocks()
        self._handle_old_best()
        self.current_stocks['best_weighted'] = self.best
        self._clean_current_stocks()
        self._clean_buy_stats()
        self.buy_stats = (
            self.buy_stats[~self.buy_stats.index.duplicated(keep='last')])
        return self.current_stocks, self.buy_stats

    def _update_lingerers(self):
        print('Updating lingerers...')
        for stock in self.current_stocks['lingerers']:
            if self._is_owned(stock):
                self._decay_holdings(stock)
            else:
                print(f'removing {stock} from lingerers')
                self.current_stocks['lingerers'].remove(stock)

    def _is_owned(self, stock):
        if stock not in self.buy_stats.index.tolist():
            print(f'{stock} not listed')
            return False
        i_own = (self.buy_stats.loc[stock, 'owned'] > 0).any()
        dm_owns = (self.buy_stats.loc[stock, 'dm'] > 0).any()
        return i_own or dm_owns

    def _decay_holdings(self, stock):
        in_accts = ['inEt', 'inFid', 'in_self_managed']
        self.buy_stats.loc[stock, in_accts] *= DECAY
        self.buy_stats[in_accts] = self.buy_stats[in_accts].round(2)

    def _insert_current_best_stocks(self):
        print('Inserting current best stocks...')
        for stock in self.current_best_stocks:
            if stock in self.current_stocks['stock_watcher']:
                if stock not in self.buy_stats.index:
                    self._append_new_stock(stock, ['inFid', 'in_self_managed'])
            elif stock in self.current_stocks['lingerers']:
                self._move_lingerer_to_best(stock)
            elif stock in self.old_best:
                self.old_best.remove(stock)
                self.best.append(stock)
            else:
                self.best.append(stock)
                self._append_new_stock(stock, ['in_self_managed'])

    def _append_new_stock(self, symbol, in_accts):
        print(f'Adding {symbol} to buy_stats')
        row = pd.DataFrame(
            {col: 0 for col in self.buy_stats.columns}, index=[symbol])
        self.buy_stats = pd.concat([self.buy_stats, row], axis=0)
        self.buy_stats.loc[symbol, in_accts + ['currentlyActive']] = 1
        total_in = (
            self.buy_stats[['inEt', 'inFid', 'in_self_managed']].sum(axis=1))
        self.buy_stats.loc[total_in == 0, 'currentlyActive'] = 0

    def _move_lingerer_to_best(self, stock):
        self.current_stocks['lingerers'].remove(stock)
        self.best.append(stock)
        self.buy_stats.loc[stock, 'inFid'] = 0
        self.buy_stats.loc[stock, 'in_self_managed'] = 1.

    def _handle_old_best(self):
        for stock in self.old_best:
            if self._is_owned(stock):
                self.current_stocks['lingerers'].append(stock)
                self._decay_holdings(stock)
            else:
                if stock in self.current_stocks['lingerers']:
                    self.current_stocks['lingerers'].remove(stock)

    def _clean_current_stocks(self):
        for group, stocks in self.current_stocks.items():
            self.current_stocks[group] = sorted(list(set(stocks)))

    def _clean_buy_stats(self):
        all_current_stocks = []
        for stocks in self.current_stocks.values():
            all_current_stocks += stocks
        for symbol in self.buy_stats.index:
            if symbol not in all_current_stocks:
                if self.buy_stats.loc[symbol, 'owned'].any():
                    print(
                        f'Owned stock:{symbol} not in current stocks. Add to '
                        f'lingerers')
                else:
                    print(
                        f'Stock {symbol} not owned and not in current stocks. '
                        f'Removing from buy_stats')
                    self.buy_stats.drop(symbol, inplace=True)
