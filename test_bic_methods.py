import pandas as pd
import numpy as np
from statsmodels.tsa.statespace.sarimax import SARIMAX

# Load data
df = pd.read_excel('ConsumoEnergiaEAgua_New.xlsx')
df['mes'] = pd.to_datetime(df['mes'])
df['Consumo'] = df['Energia'] / df['Dias']
df_clean = df[['mes', 'Consumo']].dropna()

n_size = len(df_clean)
t_size = int(3 * n_size / 4)
X_t_train = df_clean['Consumo'].iloc[:t_size].values

print(f"Training data length: {len(X_t_train)}")

# Test one model to see all available BIC methods
p, q, P, Q = 0, 1, 1, 1  # Best model from R

try:
    model = SARIMAX(X_t_train, 
                  order=(p, 1, q), 
                  seasonal_order=(P, 0, Q, 12),
                  enforce_stationarity=False,
                  enforce_invertibility=False,
                  trend='n')
    fitted_model = model.fit(disp=False, method='lbfgs')
    
    print(f"\nTesting SARIMA({p},{1},{q})({P},{0},{Q},{12}):")
    print("=" * 50)
    
    # Method 1: statsmodels default BIC
    print(f"1. fitted_model.bic: {fitted_model.bic:.6f}")
    
    # Method 2: Manual calculation
    n = len(X_t_train)
    k = p + q + P + Q + 1
    log_likelihood = fitted_model.llf
    bic_manual = -2 * log_likelihood + k * np.log(n)
    print(f"2. Manual BIC: {bic_manual:.6f}")
    
    # Method 3: Check if there's a BIC attribute in the results
    print(f"3. fitted_model.llf: {fitted_model.llf:.6f}")
    print(f"4. fitted_model.aic: {fitted_model.aic:.6f}")
    
    # Method 4: Check all available attributes
    print("\nAvailable attributes and methods:")
    print("-" * 30)
    for attr in dir(fitted_model):
        if 'bic' in attr.lower() or 'ic' in attr.lower():
            try:
                value = getattr(fitted_model, attr)
                if not callable(value):
                    print(f"{attr}: {value}")
            except:
                pass
    
    # Method 5: Check the summary
    print("\nSummary info:")
    print("-" * 30)
    summary = fitted_model.summary()
    print(summary)
    
    # Method 6: Check if there's a results object
    print("\nResults object attributes:")
    print("-" * 30)
    if hasattr(fitted_model, 'results'):
        for attr in dir(fitted_model.results):
            if 'bic' in attr.lower() or 'ic' in attr.lower():
                try:
                    value = getattr(fitted_model.results, attr)
                    if not callable(value):
                        print(f"results.{attr}: {value}")
                except:
                    pass
    
except Exception as e:
    print(f"Error: {e}") 