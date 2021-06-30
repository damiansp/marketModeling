import numpy as np
import pandas as pd


class Gene:
    def __init__(self, stocks):
        '''
        Args:
        - stocks: either:
          - dict: {'symbol', <pct_of_portfolio>} OR
          - list: ['symbol_1', ..., 'symbol_n'] (pct will be even 
              among all)
        '''
        self.TOLERANCE = 0.00000001
        if type(stocks) is list:
            stocks = self._init_stocks_from_list(stocks)
        if isinstance(stocks, dict):
            total = sum(stocks.values())
            assert abs(total - 1) < self.TOLERANCE,\
                f'Stock percentages must sum to 1 (got {total})'
        else:
            raise ValueError('stocks must be a <list> or <dict>')
        self.stocks = stocks

    @staticmethod
    def _init_stocks_from_list(stocks):
        n = len(stocks)
        stocks = {stock: 1/n for stock in stocks}
        return stocks

    def __str__(self):
        out = ''
        for stock, pct in self.stocks.items():
            if pct > self.TOLERANCE:
                out += f'{stock:5s}: {pct:.5f}\n'
        return out

    def mutate(self, sd):
        for stock, pct in self.stocks.items():
            pct += np.random.normal(scale=sd)
            pct = max(pct, 0)
            self.stocks[stock] = pct
        self.stocks = self._normalize(self.stocks)

    def _normalize(self, stocks):
        total = sum(stocks.values())
        for stock, pct in stocks.items():
            stocks[stock] = pct / total
        return stocks

    def reproduce(self, other):
        child = {}
        for stock in self.stocks:
            which = np.random.choice([self, other], 1)[0]
            val = which.stocks.get(stock, 0)
            child[stock] = val
        child = self._normalize(child)
        child = Gene(child)
        return child
