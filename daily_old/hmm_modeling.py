from collections import Counter
import multiprocessing as mp
import pickle

from hmmlearn.hmm import GaussianHMM
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import yfinance as yf


N_JOBS = 8
FIGSIZE = [12, 12]


class HMMModler:
    def __init__(
            self, symbols: dict[str, list[str]],
            state_range: range,
            reps: int,
            max_iter: int,
            model_data_path: str,
            allow_fresh_start: bool=False) -> None:
        '''Model a series of stock prices as a hidden markov model (HMM).
        Args:
        - symbols: stock tickers and time ranges to model over, formatted as:

          {'symbol': ['start_date', 'end_date']}

          where <start_date> and <end_date> are 'YYYY-MM-DD'.
        - state_range: set of possible number of states to optimize over
        - reps: number of times to restart the model
        - max_iter: max steps to allow model to iterate on until convergence
        - model_data_path: path to save best model to-date
        - allow_fresh_start: allow the modeling process to start from scratch
            if no saved models found
        '''
        self.symbols = symbols
        self.state_range = state_range
        self.reps = reps
        self.max_iter = max_iter
        self.model_data_path = model_data_path
        self.allow_fresh_start = allow_fresh_start
        self.model_data = self._load_saved_model_data()
        # model_data format:
        # {'symbol': {'logprob': 1.234, 'mod': mod_obj, 'n_states': 5]}
        # for all symbols

    def _load_saved_model_data(self):
        try:
            with open(self.model_data_path, 'rb') as f:
                model_data = pickle.load(f)
            return model_data
        except FileNotFoundError:
            print('No such file:', self.model_data_path)
            if self.allow_fresh_start:
                print('Starting from scratch...')
                model_data = {
                    symbol: {
                        'logprob': -np.inf, 'mod': None, 'n_states': None}
                    for symbol in self.symbols.keys()}
                return model_data
            raise

    def run_models(self) -> None:
        self._get_data()
        self.data.index = pd.to_datetime(self.data.index)
        self._get_daily_returns()
        self._find_best_mods()
        self._save_mods()
        self._get_all_predictions()
        self._get_mean_n_states()
        self._get_mean_state_history()
        print('Counts per state:')
        print(self.data.state.value_counts())

    def _get_data(self) -> None:
        if self._have_a_single_date_range():
            self._get_all_data_at_once()
        else:
            self._get_data_individually()

    def _have_a_single_date_range(self) -> bool:
        date_ranges = list(self.symbols.values())
        for date_range in date_ranges[1:]:
            if date_range != date_ranges[0]:
                return False
        return True

    def _get_all_data_at_once(self) -> None:
        symbols = list(self.symbols.keys())
        start, end = list(self.symbols.values())[0]
        self.data = yf.download(symbols, start=start, end=end)['Adj Close']
        
    def _get_data_individually(self) -> None:
        dfs = [
            yf
            .download(symbol, start=start, end=end)
            .rename(columns={'Adj Close': symbol})[symbol]
            for symbol, [start, end] in self.symbols.items()]
        self.data = pd.concat(dfs, axis=1)

    def _get_daily_returns(self) -> None:
        for symbol in self.symbols.keys():
            x = self.data[symbol].to_numpy()
            daily_returns = np.append([1], x[1:] / x[:-1])
            self.data[f'{symbol}_returns'] = daily_returns

    def _find_best_mods(self):
        manager = mp.Manager()
        best_mods = manager.dict()
        pool = mp.Pool(N_JOBS)
        for symbol, mod_meta in self.model_data.items():
            pool.apply_async(
                self._find_best_mod_job, args=(best_mods, symbol, mod_meta))
        pool.close()
        pool.join()
        # convert to basic dict instead of managed, to allow updates and pickle
        # serialization
        self.mods = {k: v for k, v in best_mods.items()} 

    def _find_best_mod_job(self, best_mods, symbol, mod_meta):
        daily_returns = self.data[f'{symbol}_returns']
        best_mod = self._find_best_mod(daily_returns, mod_meta)
        best_mods[symbol] = best_mod

    def _find_best_mod(self, daily_returns, mod_meta):
        daily_returns = np.reshape(daily_returns.values, [-1, 1])
        for states in self.state_range:
            for rep in range(self.reps):
                print(f'rep {rep + 1}', end='\r')
                best_meta = self._fit_model(daily_returns, states, mod_meta)
        return best_meta

    def _fit_model(self, daily_returns, states, mod_meta):
        try:
            for cov in ['spherical', 'diag', 'full']:
                mod = GaussianHMM(
                    n_components=states,
                    covariance_type=cov,
                    n_iter=self.max_iter
                ).fit(daily_returns)
                logprob = mod.score(daily_returns)
                if logprob > mod_meta['logprob']:
                    print(
                        f'\n\n     New best - States: {states} (cov: {cov})'
                        f'\n\n')
                    mod_meta['logprob'] = logprob
                    mod_meta['mod'] = mod
                    mod_meta['n_states'] = states
        except ValueError:
            pass
        return mod_meta

    def _save_mods(self):
        with open(self.model_data_path, 'wb') as f:
            pickle.dump(self.mods, f)
        print(f'Models saved to {self.model_data_path}')

    def _get_all_predictions(self):
        all_preds = []
        for symbol in self.symbols.keys():
            preds, ses, means, current_state = self._get_preds(symbol)
            today_expected = self._get_expected_value(
                symbol, current_state, means)
            all_preds.append(preds)
            pred_data = {
                'ses': ses,
                'means': means,
                'current_state': current_state,
                'today_expected': today_expected}
            self.mods[symbol].update(pred_data)
        self.data = pd.concat([self.data] + all_preds, axis=1)
        self.data['mean_exp'] = (
            self
            .data[[x for x in list(self.data) if x.endswith('_exp')]]
            .mean(axis=1))
        
    def _get_preds(self, symbol):
        returns = (
            self.data[f'{symbol}_returns'].fillna(method='ffill').dropna())
        idx = returns.index
        returns = np.reshape(returns.values, [-1, 1])
        mod = self.mods[symbol]['mod']
        states = mod.predict(returns)
        means = np.squeeze(mod.means_)
        sds = np.squeeze(np.sqrt(mod.covars_))
        preds = pd.Series(
            [means[state] for state in states], index=idx, name=f'{symbol}_exp'
        )
        ses = np.array([1.96 * sds[state] for state in states])
        return preds, ses, means, states[-1]

    def _get_mean_n_states(self):
        total_states = 0
        n = 0
        for _, data in self.mods.items():
            total_states += data['n_states']
            n += 1
        self.n_states = int(round(total_states / n))
        print('Mean states:', self.n_states)

    def save_transitions(self, path):
        transitions = self._count_transitions()
        current_state, possible_transitions, state_sum = (
            self._get_transitions_from_current_state(transitions))
        trans_df = self._normalize_transitions_to_df(
            transitions, current_state, possible_transitions, state_sum)
        print('Transtion probabilities from current state:')
        print(trans_df)
        trans_df.to_csv(path, index=False)
        print(f'Saved transition probabilities to {path}')

    def _count_transitions(self):
        transitions = Counter()
        for i in range(len(self.data.state) - 1):
            state = self.data.state[i]
            nxt = self.data.state[i + 1]
            transitions[(state, nxt)] += 1
        return transitions

    def _get_transitions_from_current_state(self, transitions):
        current_state = self.data.state[-1]
        possible_transitions = sorted(
            [k for k in transitions.keys() if k[0] == current_state])
        state_sum = 0
        for pt in possible_transitions:
            n_trans = transitions[pt]
            state_sum += n_trans
        return current_state, possible_transitions, state_sum

    @staticmethod
    def _normalize_transitions_to_df(
            transitions, current_state, possible_transitions, state_sum):
        data = []
        for pt in possible_transitions:
            n_trans = transitions[pt]
            data.append([current_state, pt[1], n_trans / state_sum])
        trans_df = pd.DataFrame(
            data, columns=['current_state', 'next_state', 'prob'])
        return trans_df

    def _get_expected_value(self, symbol, current_state, means):
        mod = self.mods[symbol]['mod']
        return np.dot(mod.transmat_[current_state, :], means)
    
    def _get_mean_state_history(self, n_states=None):
        if n_states is None:
            n_states = self.n_states
        try:
            self.data['state'] = pd.qcut(
                self.data.mean_exp,
                self.n_states,
                retbins=False,
                duplicates='drop',
                labels=range(n_states))
        except ValueError:
            # may have to reduce n_states if duplicate cut points
            self._get_mean_state_history(n_states - 1)

    def save_state_data(self, path):
        print('State data:')
        print(self.data.tail())
        self.data.to_csv(path)
        print('Saved state data to', path)
        
    def plot_models(self, last_n_days):
        for symbol in self.symbols.keys():
            self._plot_mod(symbol, last_n_days)
        plt.figure(figsize=FIGSIZE)
        plt.subplot(2, 1, 1)
        self._plot_states(last_n_days)
        plt.subplot(2, 1, 2)
        self._plot_binned_states(last_n_days)
        plt.show()

    def _plot_mod(self, symbol, last_n_days):
        plt.figure(figsize=FIGSIZE)
        plt.subplot(3, 1, 1)
        self._plot_full_history(symbol)
        plt.subplot(3, 1, 2)
        self._plot_predictions(symbol, last_n_days)
        plt.subplot(3, 1, 3)
        self._plot_returns(symbol, last_n_days)
        plt.show()
    
    def _plot_full_history(self, symbol):
        plt.plot(self.data[f'{symbol}_returns'], label='daily returns')
        plt.plot(self.data[f'{symbol}_exp'], label='preds')
        plt.legend()
        plt.title(symbol)

    def _plot_predictions(self, symbol, last_n_days):
        expected = self.mods[symbol]['today_expected']
        ses = self.mods[symbol]['ses']
        plt.plot(
            self.data[f'{symbol}_returns'],
            label=f'daily returns\n(Today exp: {expected:.6f})')
        plt.plot(
            self.data[f'{symbol}_exp'],
            label='preds',
            color='orange',
            linewidth=3) 
        plt.plot(self.data[f'{symbol}_exp'] + ses, color='orange', alpha=0.8)
        plt.plot(self.data[f'{symbol}_exp'] - ses, color='orange', alpha=0.8)
        plt.axhline(y=1, color='k')
        plt.ylim([0.95, 1.05])
        plt.xlim([self.data.index[-last_n_days], self.data.index[-1]])
        plt.legend()

    def _plot_returns(self, symbol, last_n_days):
        plt.plot(self.data[symbol])
        plt.xlim([self.data.index[-last_n_days], self.data.index[-1]])
        sub = self.data.iloc[-last_n_days:, :]
        plt.ylim(0.95 * sub[symbol].min(), 1.05 * sub[symbol].max())

    def _plot_states(self, last_n_days):
        pred_cols = [col for col in list(self.data) if col.endswith('_exp')]
        for col in pred_cols:
            if col == 'mean_exp':
                linewidth = 3
                alpha = 1
            else:
                linewidth = 1
                alpha = 0.5
            plt.plot(
                self.data[col], alpha=alpha, linewidth=linewidth, label=col)
        plt.axhline(y=1, color='k')
        plt.xlim([self.data.index[-last_n_days], self.data.index[-1]])
        plt.ylim([0.995, 1.005])
        plt.legend()

    def _plot_binned_states(self, last_n_days):
        plt.plot(self.data.state)
        plt.xlim([self.data.index[-last_n_days], self.data.index[-1]])
