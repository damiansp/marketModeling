#!/usr/bin/env python3
import matplotlib.pyplot as plt
import numpy as np
import os
import pandas as pd
import sys
import tensorflow as tf
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from tensorflow.python.framework import ops

plt.style.use('ggplot')

DATA_PATH = '../data/'
DROPOUT_RATE = 0.1
DROPOUT_RATE = 0.1
ETA = 0.001 # Learning rate
EPOCHS = 2000
BATCH_SIZE = 10


def read(filepath, pred=False):
    dat = pd.read_csv(filepath)
    if not pred:
        dat['buyDate'] = pd.to_datetime(dat['buyDate'])
        dat['sellDate'] = pd.to_datetime(dat['sellDate'])
    return dat

def drop_missing(dfs, y_col):
    X = dfs[0].copy()
    # cols
    drop_cols = [col for col in list(X)
                 if type(col[0]) == np.float and col.isnan().all()]
    X = X.drop(drop_cols, axis=1)
    X2 = dfs[1].drop(drop_cols, axis=1)
    return X.loc[np.isnan(dat[y_col]) == False, :], X2

def split_xy(df, y_col, first_X):
    y = df[y_col]
    start_col = list(df).index(first_X)
    X = df.iloc[:, start_col:]
    return X, y

def one_hot(df):
    X = df.copy()
    X[['mfNone', 'mfNow', 'mfStart']] = pd.get_dummies(X['MF'])
    X = X.drop('MF', axis=1)
    return X

def median_fill(df):
    X = df.copy()

    for col in list(X):
        try:
            X[col][np.isnan(X[col])] = np.nanmedian(X[col])
        except:
            print('Could not fill column', col)
    return X

dat = read(DATA_PATH + 'buyStats.csv')
new_dat = read(DATA_PATH + 'Multistock.csv', pred=True)
dat, new_dat = drop_missing([dat, new_dat], 'd30Gains')
X, y = split_xy(dat, 'd30Gains', 'MF')
X_new = new_dat.iloc[:, 2:]
stocks = new_dat['stock']

print('Checking that DataFrames are consistent...')
if list(X) == list(X_new):
    print('OK!')
else:
    print([(x, xn) for (x, xn) in zip(list(X), list(X_new))])
    print('Error in table alignment')
    sys.exit(-1)

X = one_hot(X)
X_new = one_hot(X_new)

# ONLY DO UNTIL DATA EXIST!
if X['grossMarQ'].isnull().all() == False:
    print('You have "grossMarQ" data, update code')
else:
    X = X.drop('grossMarQ', axis=1)
    X_new = X_new.drop('grossMarQ', axis=1)

X = median_fill(X)
X_new = median_fill(X_new)

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2)

# Reindex
X_train.index = range(len(y_train))
y_train.index = range(len(y_train))

scaler = StandardScaler()
X_train = scaler.fit_transform(X_train)
X_test = scaler.transform(X_test)
X_new  = scaler.transform(X_new)


# Create Neural Net-----------------------------------------------------------
# Create a fully-connected layer of neurons using dropout regularization
def make_drop_layer(in_layer,
                    n_out,
                    activation,
                    kernel_initializer,
                    dropout_rate,
                    training):
        h = tf.layers.dense(in_layer,
                            n_out,
                            activation=activation,
                            kernel_initializer=kernel_initializer)
        h_drop = tf.layers.dropout(h, rate=dropout_rate, training=training)
        return h_drop

ops.reset_default_graph()
g = tf.get_default_graph()

N, D = X_train.shape

X = tf.placeholder(tf.float32, shape=[None, D], name='X')
y = tf.placeholder(tf.float32, shape=None, name='y')
training = tf.placeholder_with_default(False, shape=(), name='training')

X_drop = tf.layers.dropout(X, DROPOUT_RATE, training=training)
n_neurons = [D, 27, 20, 15, 10, 5, 1]

with tf.name_scope('dnn'):
    # He intialization randomly initalizes weigths and their variances in
    # a way that minimizes the likelihood of vanishing/exploding gradients
    he_init = tf.contrib.layers.variance_scaling_initializer()
    current_in = X

    for layer in range(1, len(n_neurons)):
        current_in = make_drop_layer(
            in_layer=current_in,
            n_out=n_neurons[layer],
            activation=tf.nn.tanh if (layer + 1) < len(n_neurons) else None,
            kernel_initializer=he_init,
            dropout_rate=DROPOUT_RATE,
            training=training)

    preds = tf.nn.relu(current_in)

with tf.name_scope('cost'):
    # MSE
    cost = tf.reduce_sum(tf.pow(preds - y, 2))/(2 * N)

with tf.name_scope('train'):
    eta0 = ETA # initial learning rate
    decay_steps = 100  # eta updates after this many epochs
    decay_rate = 0.99    # factor to change eta by
    global_step = tf.Variable(0, trainable=False, name='global_step')
    eta = tf.train.exponential_decay(
        eta0, global_step, decay_steps, decay_rate, name='eta')
    optimizer = tf.train.AdamOptimizer(eta).minimize(
        cost, global_step=global_step)
    
train_err = []
test_err = []
saver = tf.train.Saver()

with tf.Session() as s:
    s.run(tf.global_variables_initializer())

    for epoch in range(EPOCHS):
        idxs = np.random.permutation(range(N))
        n_batches = len(idxs) // BATCH_SIZE

        for batch in range(n_batches):
            idx = idxs[batch * BATCH_SIZE : (batch + 1) * BATCH_SIZE]
            s.run(optimizer, feed_dict={training: True,
                                        X: X_train[idx, :],
                                        y: y_train[idx]})
            err_train = cost.eval(feed_dict={X: X_train[idx, :],
                                             y: y_train[idx]})
            err_test = (cost.eval(feed_dict={X: X_test, y: y_test})
                        * (N) / (len(y_test))) # rescale for diff y length
            train_err.append(err_train)
            test_err.append(err_test)

        if epoch % 100 == 0:
            print(
                '%3d: Train error: %.5f\tTest error: %.9f'
                %(epoch, err_train, err_test))

    save_path = saver.save(s, './nn_mod.ckpt')

plt.plot(train_err, 'k-', label='train')
plt.plot(test_err, 'r-', label='test')
plt.legend(loc='best')
plt.xlabel('Epochs')
plt.ylabel('MSE')
plt.show()

s = tf.Session()
saver = tf.train.Saver()
saver.restore(s, './nn_mod.ckpt')
new_preds = s.run(preds, feed_dict={X: X_new})

final_out = []
for stock, pred in zip(stocks, new_preds.reshape(new_preds.shape[0])):
    final_out.append([stock, pred])

out = pd.DataFrame(columns=['Stock', 'Prediction'], data=final_out)
out = out.sort_values('Stock', ascending=False)
print(out.head(len(stocks)))
