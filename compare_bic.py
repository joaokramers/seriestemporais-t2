# ========================================================
# BIC Comparison Script
# ========================================================
# Compare different BIC calculation methods
# ========================================================

import pandas as pd
import numpy as np
from statsmodels.tsa.statespace.sarimax import SARIMAX

def load_data():
    """Load and prepare data"""
    energia = pd.read_excel("ConsumoEnergiaEAgua_New.xlsx")
    energia['mes'] = pd.to_datetime(energia['mes'])
    energia['Consumo'] = energia['Energia'] / energia['Dias']
    
    # Clean data - only remove NaN from the columns we need
    energia = energia[['mes', 'Consumo']].dropna()
    
    n_size = len(energia)
    t_size = int(3 * n_size / 4)
    X_t_train = energia['Consumo'].iloc[:t_size].values
    
    return X_t_train

def calculate_bic_methods(data, p, q, P, Q, s=12):
    """
    Calculate BIC using different methods for comparison
    """
    try:
        # Fit model
        model = SARIMAX(data, 
                      order=(p, 1, q), 
                      seasonal_order=(P, 0, Q, s),
                      enforce_stationarity=False,
                      enforce_invertibility=False,
                      trend='n')
        fitted_model = model.fit(disp=False, method='lbfgs')
        
        n = len(data)
        k = p + q + P + Q + 1  # parameters + variance
        
        # Method 1: statsmodels default BIC
        bic_statsmodels = fitted_model.bic
        
        # Method 2: Manual calculation similar to R
        log_likelihood = fitted_model.llf
        bic_manual = -2 * log_likelihood + k * np.log(n)
        
        # Method 3: AIC for comparison
        aic = fitted_model.aic
        
        # Method 4: AICc (corrected AIC)
        aicc = aic + (2 * k * (k + 1)) / (n - k - 1)
        
        return {
            'p': p, 'q': q, 'P': P, 'Q': Q,
            'BIC_statsmodels': bic_statsmodels,
            'BIC_manual': bic_manual,
            'AIC': aic,
            'AICc': aicc,
            'log_likelihood': log_likelihood,
            'n_params': k,
            'n_obs': n
        }
        
    except:
        return None

def main():
    print("Loading data...")
    data = load_data()
    
    print(f"Data length: {len(data)}")
    print(f"Data range: {data.min():.2f} to {data.max():.2f}")
    print(f"Data mean: {data.mean():.2f}")
    print()
    
    # Test specific models from R results
    test_models = [
        (0, 1, 1, 1),  # Best model from R
        (2, 3, 0, 0),  # Second best from R
        (0, 1, 2, 0),  # Third best from R
        (0, 2, 1, 1),  # Fourth best from R
        (1, 1, 1, 1),  # Fifth best from R
    ]
    
    results = []
    
    print("Testing specific models from R results:")
    print("=" * 80)
    print(f"{'Model':<12} {'BIC_R':<12} {'BIC_Python':<12} {'Diff':<12} {'AIC':<12}")
    print("-" * 80)
    
    # R BIC values from your output
    r_bic_values = [3.654765, 3.663542, 3.668547, 3.668808, 3.670473]
    
    for i, (p, q, P, Q) in enumerate(test_models):
        result = calculate_bic_methods(data, p, q, P, Q)
        
        if result:
            r_bic = r_bic_values[i]
            python_bic = result['BIC_manual']
            diff = python_bic - r_bic
            
            print(f"SARIMA({p},{1},{q})({P},{0},{Q},{12}): {r_bic:<12.6f} {python_bic:<12.6f} {diff:<12.6f} {result['AIC']:<12.6f}")
            
            results.append({
                'model': f"SARIMA({p},{1},{q})({P},{0},{Q},{12})",
                'r_bic': r_bic,
                'python_bic': python_bic,
                'difference': diff,
                'aic': result['AIC'],
                'log_likelihood': result['log_likelihood'],
                'n_params': result['n_params']
            })
    
    print("=" * 80)
    
    # Save comparison results
    comparison_df = pd.DataFrame(results)
    comparison_df.to_csv('bic_comparison.csv', index=False)
    print("\nComparison results saved to 'bic_comparison.csv'")
    
    # Analyze differences
    print("\nAnalysis of differences:")
    print(f"Mean difference (Python - R): {comparison_df['difference'].mean():.6f}")
    print(f"Std difference: {comparison_df['difference'].std():.6f}")
    print(f"Min difference: {comparison_df['difference'].min():.6f}")
    print(f"Max difference: {comparison_df['difference'].max():.6f}")
    
    # Check if differences are consistent
    if comparison_df['difference'].std() < 0.1:
        print("\nDifferences are relatively consistent - likely due to:")
        print("1. Different optimization algorithms")
        print("2. Different convergence criteria")
        print("3. Different parameter constraints")
        print("4. Different handling of initial values")
    else:
        print("\nDifferences vary significantly - may indicate:")
        print("1. Different model specifications")
        print("2. Different data preprocessing")
        print("3. Different seasonal handling")

if __name__ == "__main__":
    main() 