import numpy as np, pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error
from argparse import ArgumentParser

def parse_args():
    parser = ArgumentParser()
    parser.add_argument('--split_dir', type=str)
    return parser.parse_args()

args = parse_args()

FILE = args.split_dir
EXCLUDE = {'subject_id','hadm_id','LOS'}
TEST_SIZE, VAL_SIZE = 0.20, 0.20
RANDOM_STATE = 42

def rmse(y_true, y_pred):
    return np.sqrt(mean_squared_error(y_true, y_pred))

df = pd.read_csv(FILE)
X = df[[c for c in df.columns if c not in EXCLUDE]].astype(float).values
y = df['LOS'].values

X_tmp, X_test, y_tmp, y_test = train_test_split(X, y, test_size=TEST_SIZE, random_state=RANDOM_STATE)
X_tr, X_val, y_tr, y_val = train_test_split(X_tmp, y_tmp, test_size=VAL_SIZE, random_state=RANDOM_STATE)

X_trv, y_trv = np.vstack([X_tr, X_val]), np.concatenate([y_tr, y_val])
sx = StandardScaler().fit(X_trv)
final = LinearRegression().fit(sx.transform(X_trv), y_trv)
t = rmse(y_test, final.predict(sx.transform(X_test)))

print(f"Linear Regression | testRMSE={t:.4f}")
