from math import ceil


class Batcher:
    def __init__(self, symbols, market_indices):
        assert len(market_indices) <= 7, 'max market_indices is 7'
        self.symbols = symbols
        self.market_indices = market_indices

    def get_batch_from_weekday(self, weekday, include_weekends=False):
        '''
        weekday (int): datetime.date().weekday() value on [0, 6] (0: Mon)
        '''
        n_batches = 7 if include_weekends else 5
        n_per_batch = ceil(len(self.symbols) / n_batches)
        batch = self.symbols[
            (weekday * n_per_batch):(weekday*n_per_batch + n_per_batch)]
        n_inds = len(self.market_indices)
        ind = self.market_indices[weekday % n_inds]
        batch.append(ind)
        return batch
