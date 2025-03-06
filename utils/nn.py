import numpy as np


class FFNN:
    def __init__(
            self,
            n_in,
            hidden_nodes: list,
            n_out: int = 1,
            use_bias: bool = True,
            Ws = None):
        '''Params
        - hidden_nodes: list with n nodes per layer
        '''
        self.n_in = n_in
        self.hidden_nodes = np.array(hidden_nodes)
        self.use_bias = use_bias
        self.n_out = n_out
        self.Ws = Ws if Ws is not None else self._init_weights()

    def _init_weights(self):
        n_in = self.n_in
        if self.use_bias:
            n_in += 1
        W1 = np.random.normal(size=(n_in, self.hidden_nodes[0]))
        Ws = [W1]
        if len(self.hidden_nodes) > 1:
            for i, n_nodes in enumerate(self.hidden_nodes[1:], 1):
                n_in = self.hidden_nodes[i - 1]
                if self.use_bias:
                    n_in += 1
                W = np.random.normal(size=(n_in, n_nodes))
                Ws.append(W)
        # output
        n_in = self.hidden_nodes[-1]
        if self.use_bias:
            n_in += 1
        W = np.random.normal(size=(n_in, self.n_out))
        Ws.append(W)
        return Ws

    def get_state(self):
        state = {
            'n_in': self.n_in,
            'hidden_nodes': self.hidden_nodes,
            'n_out': self.n_out,
            'use_bias': self.use_bias,
            'Ws': self.Ws}
        return state

    def forward_pass(self, X, activation, activation_out):
        activation = {'relu': self._relu}[activation]
        activation_out = {'sigmoid': self._sigmoid}[activation_out]
        X = np.array(X)
        if len(X.shape) == 1:
            X = np.array([X])
        for W in self.Ws[:-1]:
            if self.use_bias:
                X = self._append_bias(X)
            X = X @ W
            X = activation(X)
        # output layer
        if self.use_bias:
            X = self._append_bias(X)
        X = X @ self.Ws[-1]
        X = activation_out(X)
        return X

    def _append_bias(self, X):
        X = self._append_col(X, np.ones, to_front=True)
        return X
    
    @staticmethod
    def _append_col(X, col_func, to_front=False, **kwargs):
        n_rows = X.shape[0]
        if col_func == np.random.normal:
            new_col = col_func(size=(n_rows, 1), **kwargs)
        else:
            new_col = col_func(shape=(n_rows, 1), **kwargs)
        if to_front:
            X = np.concatenate([new_col, X], axis=1)
        else:
            X = np.concatenate([X, new_col], axis=1)
        return X

    @staticmethod
    def _relu(X):
        return(X.clip(0, None))
    
    @staticmethod
    def _sigmoid(X):
        return 1 / (1 + np.exp(-X))
    
    def mutate(self, scale: float):
        '''Randomly mutate weights
        Weights will be multiplied by ~N(1, scale)
        In early iterations, it can be large (say 0.5), but should
        gradually diminish as models start to settle
        Parameters:
        - scale: standard deviations
        '''
        for i, W in enumerate(self.Ws):
            noise = np.random.normal(loc=1, scale=scale, size=W.shape)
            W = np.multiply(W, noise)
            self.Ws[i] = W

    def add_input(self, scale=0.01):
        self.n_in += 1
        self.Ws[0] = self._append_row(self.Ws[0], np.random.normal, scale=scale)
        
    @staticmethod
    def _append_row(X, row_func, **kwargs):
        n_cols = X.shape[1]
        new_row = row_func(size=(1, n_cols), **kwargs)
        X = np.concatenate([X, new_row])
        return X

    def add_node(self, scale=0.01):
        # add to the first layer that is < n_in
        for i, W in enumerate(self.Ws):
            n_out = W.shape[1]
            if n_out < self.n_in:
                try:
                    print(
                        f'Attempting to add node to W[{i}] '
                        f'({n_out} -> {n_out + 1})')
                    self.Ws[i + 1] = self._append_row(
                        self.Ws[i + 1], np.random.normal, scale=scale)
                    self.Ws[i] = self._append_col(
                        W, np.random.normal, scale=scale)
                    print('Success.')
                except IndexError:
                    print(
                        f'Failed. '
                        f'Initializing first node in new layer ({i + 1})')
                    if self.use_bias:
                        n_out += 1
                    new_W = np.ones((n_out, 1))
                    new_W[0, 0] = 0.
                    self.Ws.append(new_W)
                return
