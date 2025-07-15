from hyperimpute.plugins.imputers import Imputers
from warnings import filterwarnings

def hyperimpute_imp(X, method, seed, **kwargs):
    filterwarnings('ignore')
    imputer = Imputers().get(method)
    imputer.__init__(random_state=seed, **kwargs)
    X_imputed = imputer.fit_transform(X.copy())
    return X_imputed
