# Special thanks to @Peter's answer here:
# https://math.stackexchange.com/questions/1832177/sigmoid-function-with-fixed-bounds-and-variable-steepness-partially-solved
import numpy as np


def bounded_curve(x, t, k, updown='up'):
    ''''Sigmoid curve over the interval x = [0, 1] and y = [0, 1] with min/max
    set to exactley 0/1 (not approaching asymptotically).
    Args:
    - x (float on [0, 1], or np.ndarray of value on [0, 1])
    - t (float on [0, 1]): controls the point on the x-axis where y = 0.5
    - k (float on (0, inf)): controls direction and amount of curvature:
      - k -> inf: sigmoid becomes more vertical (approaches the step
        function)
      - k = 1: minimal curvature for selected t: straight diagonal if t = 0.5
      - k -> 0: sigmoid reflected about x=y, becomes more horizontal
        (approaches step function)
    - updown (str): 'up' | 'down':
      - up: typical upward sloping (left to right) curve
      - down: downward sloping curve ("backward sigmoid")
    '''
    if updown not in ['up', 'down']:
        raise ValueError(f'"updown" must be "up" or "down", got {updown}')
    y = 1 / (1 + x**(np.log(2) / np.log(t)) - 1) ** k
    if updown == 'down':
        y = 1 - y
    return y


def bounded_sigmoid(x, t, k, updown='up'):
    ''''Sigmoid curve over the interval x = [0, 1] and y = [0, 1] with min/max
    set to exactley 0/1 (not approaching asymptotically).
    Args:
    - x (float on [0, 1], or np.ndarray of value on [0, 1])
    - t (float on [0, 1]): controls the point on the x-axis where y = 0.5
    - k (float on (0, inf)): controls direction and amount of curvature:
      - k -> inf: sigmoid becomes more vertical (approaches the step
        function)
      - k = 1: minimal curvature for selected t: straight diagonal if t = 0.5
      - k -> 0: sigmoid reflected about x=y, becomes more horizontal
        (approaches step function)
    - updown (str): 'up' | 'down':
      - up: typical upward sloping (left to right) curve
      - down: downward sloping curve ("backward sigmoid")
    '''
    if updown not in ['up', 'down']:
        raise ValueError(f'"updown" must be "up" or "down", got {updown}')
    y = 1 / (1 + (x**(np.log(2) / np.log(t)) - 1) ** k)
    if updown == 'down':
        y = 1 - y
    return y


# Test
if __name__ == '__main__':
    import matplotlib.pyplot as plt
    
    xs = np.linspace(0, 1, 101)
    for i, t in enumerate([0.33, 0.5, 0.75]):
        plt.subplot(1, 3, i + 1)
        for k in [0.125, 0.25, 0.5, 1, 2, 4, 8]:
            ys = bounded_sigmoid(xs, t, k, 'down')
            plt.plot(xs, ys, label=f'k={k}')
        plt.title(f't = {t}')
        plt.legend()
    plt.show()
                     
    
    
