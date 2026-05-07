import numpy as np


def entropy(x):
    if x==0.:
        return 0.
    elif x==1.:
        return 0.
    return -x * np.log2(x) - (1-x) * np.log2(1-x)


def secret_fraction(w):
    return max(1 - 2. * entropy((1.-w)/2.), 0.)


def secret_key_rate(pmf, w_func):
    aver_w = get_mean_werner(pmf, w_func)
    aver_w = min(aver_w, 1.) # avoid w > 1
    aver_t = get_mean_waiting_time(pmf)

    key_rate = 1/aver_t * secret_fraction(aver_w)
    if key_rate < 0.:
        key_rate = 0.
    return key_rate


def get_mean_werner(pmf, w_func):
    w_func = np.where(np.isnan(w_func), 0., w_func)
    coverage = np.sum(pmf)
    if coverage <= 0:
        return 0.  # to prevent nan corrupts the optimization result
    else:
        aver_w = np.sum(pmf * w_func) / coverage
    return aver_w


def get_mean_waiting_time(pmf):
    coverage = np.sum(pmf)
    if coverage <= 0:
        return np.inf  # to prevent nan corrupts the optimization result
    else:
        return (1 - coverage) / coverage * len(pmf) + np.sum(pmf * np.arange(len(pmf)))/coverage