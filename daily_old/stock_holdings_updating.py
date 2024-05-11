import pandas as pd


DECAY = 0.8


class StockHoldingsUpdater:
    def __init__(
            self, current_stocks, current_best_stocks, buy_stats, fid_max):
        self.current_stocks = current_stocks
        self.current_best_stocks = current_best_stocks
        self.buy_stats = buy_stats
        self.fid_max = fid_max
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
        for stock in self.current_stocks['lingerers']:
            if self._is_owned(stock):
                self._decay_holdings(stock)
            else:
                print(f'removing {stock} from lingerers')
                self.current_stocks['lingerers'].remove(stock)

    def _is_owned(self, stock):
        #if stock not in self.buy_stats.stock.tolist():
        if stock not in self.buy_stats.index.tolist():
            print(f'{stock} not listed')
            return False
        return (
            #self.buy_stats.loc[self.buy_stats.stock == stock, 'Owned'] > 0
            self.buy_stats.loc[stock, 'Owned'] > 0
        ).any()
        
    def _decay_holdings(self, stock):
        #self.buy_stats.loc[self.buy_stats.stock == stock, 'inFid'] *= DECAY
        self.buy_stats.loc[stock, 'inFid'] *= DECAY
        self.buy_stats.loc[
            #self.buy_stats.stock == stock, 'in_self_managed'
            stock, 'in_self_managed'
        ] *= DECAY
        self.buy_stats.inFid = self.buy_stats.inFid.round(2)
        self.buy_stats.in_self_managed = (
            self.buy_stats.in_self_managed.round(2))

    def _insert_current_best_stocks(self):
        for stock in self.current_best_stocks:
            if (stock in self.current_stocks['blast_off']
                or stock in self.current_stocks['stock_watcher']):
                if stock not in self.buy_stats.index:
                    self._append_new_stock(stock, 'et')
            elif stock in self.current_stocks['lingerers']:
                self._move_lingerer_to_best(stock)
            elif stock in self.old_best:
                self.old_best.remove(stock)
                self.best.append(stock)
            else:
                self.best.append(stock)
                self._append_new_stock(stock, 'tdam')

    def _move_lingerer_to_best(self, stock):
        self.current_stocks['lingerers'].remove(stock)
        self.best.append(stock)
        self.buy_stats.loc[stock, 'inFid'] = self.fid_max
        self.buy_stats.loc[stock, 'in_self_managed'] = 1.

    def _append_new_stock(self, symbol, acct):
        print(f'Adding {symbol} to buy_stats')
        row = pd.DataFrame(
            {col: 0 for col in self.buy_stats.columns}, index=[symbol])
        self.buy_stats.to_csv('/tmp/test.csv')
        self.buy_stats = pd.concat([self.buy_stats, row], axis=0)
        if acct == 'et':
            self.buy_stats.loc[symbol, 'inEt'] = 1
        else:
            self.buy_stats.loc[symbol, 'inFid'] = self.fid_max
            self.buy_stats.loc[symbol, 'in_self_managed'] = 1
            ###
            self.buy_stats.loc[symbol, 'currentlyActive'] = 1
            ###
        ###
        total_in = self.buy_stats[['inEt', 'inFid', 'in_self_managed']].sum(axis=1)
        self.buy_stats.loc[total_in == 0, 'currentlyActive'] = 0
        ###
                               
        
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
                if (self.buy_stats
                    .loc[symbol, 'Owned']
                    .any()):
                    print(
                        f'Owned stock:{symbol} not in current stocks. Add to '
                        f'lingerers')
                else:
                    print(
                        f'Stock {symbol} not owned and not in current stocks. '
                        f'Removing from buy_stats')
                    self.buy_stats.drop(symbol, inplace=True)
