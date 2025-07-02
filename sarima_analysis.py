# ========================================================
# A household electricity consumption
# ========================================================
# last update: 20/May/2025
# 
# Time series analysis using the SARIMA model
#
#
# Raul Matsushita
# ========================================================

# ========================================================
# Load libraries
# ========================================================

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from statsmodels.tsa.stattools import adfuller
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
from statsmodels.tsa.statespace.sarimax import SARIMAX
from statsmodels.stats.diagnostic import acorr_ljungbox
from scipy.stats import shapiro
import warnings
warnings.filterwarnings('ignore')

# Set style for plots
plt.style.use('seaborn-v0_8')
plt.rcParams['font.family'] = 'Times New Roman'
plt.rcParams['font.size'] = 12

# ========================================================
# Data preparation
# ========================================================

# Read the data
energia = pd.read_excel("ConsumoEnergiaEAgua_New.xlsx")
energia['mes'] = pd.to_datetime(energia['mes'])
energia['Consumo'] = energia['Energia'] / energia['Dias']

# Clean data - only remove NaN from the columns we need
energia = energia[['mes', 'Consumo']].dropna()

# ========================================================
# Figure 1 - Time series plot
# ========================================================

plt.figure(figsize=(12, 6))
plt.plot(energia['mes'], energia['Consumo'], linewidth=1.2)
plt.title('Consumo kWh/dia', fontsize=16, fontweight='bold')
plt.xlabel('t', fontsize=14, style='italic')
plt.ylabel('Y_t', fontsize=14, style='italic')
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.show()

# ========================================================
# Sample ACF and Partial ACF
# ========================================================

Y_t = energia['Consumo'].values

# ACF plot
plt.figure(figsize=(12, 4))
plot_acf(Y_t, lags=40, alpha=0.05, title='FAC da série original')
plt.xlabel('h', fontsize=12, style='italic')
plt.ylabel('ρ_h', fontsize=12)
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.show()

# PACF plot
plt.figure(figsize=(12, 4))
plot_pacf(Y_t, lags=40, alpha=0.05, title='FACP da série original')
plt.xlabel('h', fontsize=12, style='italic')
plt.ylabel('φ_hh', fontsize=12)
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.show()

# ========================================================
# Testing first order nonstationarity
# ========================================================

adf_result = adfuller(Y_t, regression='ct', autolag='AIC')
print("Augmented Dickey-Fuller Test:")
print(f"ADF Statistic: {adf_result[0]:.6f}")
print(f"p-value: {adf_result[1]:.6f}")
print(f"Critical values:")
for key, value in adf_result[4].items():
    print(f"\t{key}: {value:.3f}")

# ========================================================
# Differentiation
# ========================================================

X_t = np.diff(energia['Consumo'])
energia['dif'] = np.concatenate([[np.nan], X_t])

# Plot of differences
plt.figure(figsize=(12, 6))
plt.plot(energia['mes'], energia['dif'], linewidth=1.2)
plt.axhline(y=0, color='blue', linestyle='--', linewidth=1)
plt.title('Variação do consumo (kWh)', fontsize=16, fontweight='bold')
plt.xlabel('t', fontsize=14, style='italic')
plt.ylabel('∇Y_t', fontsize=14)
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.show()

# ========================================================
# ACF and PACF of differenced series
# ========================================================

# ACF of differenced series
plt.figure(figsize=(12, 4))
plot_acf(X_t, lags=60, alpha=0.05, title='FAC da primeira diferença')
plt.xlabel('h', fontsize=12, style='italic')
plt.ylabel('ρ_h', fontsize=12)
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.show()

# PACF of differenced series
plt.figure(figsize=(12, 4))
plot_pacf(X_t, lags=60, alpha=0.05, title='FACP da primeira diferença')
plt.xlabel('h', fontsize=12, style='italic')
plt.ylabel('φ_hh', fontsize=12)
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.show()

# ========================================================
# Training and Model identification
# ========================================================

n_size = len(energia)
t_size = int(3 * n_size / 4)
X_t_train = energia['Consumo'].iloc[:t_size].values

# Grid search for best SARIMA model
def grid_search_sarima(data, max_p=4, max_q=4, max_P=2, max_Q=2, s=12):
    """
    Perform grid search to find the best SARIMA model parameters
    """
    best_bic = np.inf
    best_params = None
    results = []
    model_count = 0
    
    for p in range(max_p + 1):
        for q in range(max_q + 1):
            for P in range(max_P + 1):
                for Q in range(max_Q + 1):
                    model_count += 1
                    try:
                        model = SARIMAX(data, 
                                      order=(p, 1, q), 
                                      seasonal_order=(P, 0, Q, s),
                                      enforce_stationarity=False,
                                      enforce_invertibility=False)
                        fitted_model = model.fit(disp=False)
                        
                        # Use BIC directly from the model (like R's TSA package)
                        bic = fitted_model.bic
                        
                        results.append({
                            'ID': model_count,
                            'BIC': bic,
                            'p': p, 'q': q, 'P': P, 'Q': Q,
                            'log_likelihood': fitted_model.llf,
                            'n_params': p + q + P + Q + 1
                        })
                        
                        if bic < best_bic:
                            best_bic = bic
                            best_params = (p, q, P, Q)
                            
                    except:
                        continue
    
    return pd.DataFrame(results).sort_values('BIC'), best_params

print("Performing grid search for best SARIMA model...")
bic_results, best_params = grid_search_sarima(X_t_train)

print("\nTop 10 models by BIC:")
print("=" * 50)
print(f"{'ID':<3} {'BIC':<12} {'p':<3} {'q':<3} {'P':<3} {'Q':<3}")
print("-" * 50)
for i, row in bic_results.head(10).iterrows():
    print(f"{row['ID']:<3} {row['BIC']:<12.6f} {row['p']:<3} {row['q']:<3} {row['P']:<3} {row['Q']:<3}")
print("=" * 50)

# ========================================================
# Fit the best model
# ========================================================

best_p, best_q, best_P, best_Q = best_params
print(f"\nBest model parameters: SARIMA({best_p},1,{best_q})({best_P},0,{best_Q},12)")

# Fit the best model
best_model = SARIMAX(X_t_train, 
                    order=(best_p, 1, best_q), 
                    seasonal_order=(best_P, 0, best_Q, 12),
                    enforce_stationarity=False,
                    enforce_invertibility=False)
fitted_model = best_model.fit(disp=False)

print("\nModel Summary:")
print(fitted_model.summary())

# ========================================================
# Model diagnostics
# ========================================================

# Ljung-Box test
residuals = fitted_model.resid
lb_results = acorr_ljungbox(residuals, lags=30, return_df=True)
print("\nLjung-Box Test Results:")
print(lb_results)

# Residual ACF and PACF
plt.figure(figsize=(12, 4))
plot_acf(residuals, lags=60, alpha=0.05, title='FAC residual')
plt.xlabel('h', fontsize=12, style='italic')
plt.ylabel('ρ_h', fontsize=12)
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.show()

plt.figure(figsize=(12, 4))
plot_pacf(residuals, lags=60, alpha=0.05, title='FACP residual')
plt.xlabel('h', fontsize=12, style='italic')
plt.ylabel('φ_hh', fontsize=12)
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.show()

# Normality test
shapiro_stat, shapiro_p = shapiro(residuals)
print(f"\nShapiro-Wilk Test for Normality:")
print(f"Statistic: {shapiro_stat:.6f}")
print(f"p-value: {shapiro_p:.6f}")

# ========================================================
# Model Validation (Out of sample)
# ========================================================

# Fit model on full dataset
full_model = SARIMAX(energia['Consumo'], 
                    order=(best_p, 1, best_q), 
                    seasonal_order=(best_P, 0, best_Q, 12),
                    enforce_stationarity=False,
                    enforce_invertibility=False)
full_fitted = full_model.fit(disp=False)

# Get fitted values and residuals
energia['X_hat'] = energia['Consumo'] - full_fitted.resid
energia['X_res'] = full_fitted.resid

# Set training period fitted values to NaN for out-of-sample validation
energia.loc[:t_size-1, 'X_hat'] = np.nan

# Plot with fitted values
plt.figure(figsize=(12, 6))
plt.plot(energia['mes'], energia['Consumo'], linewidth=1.2, label='Original')
plt.plot(energia['mes'], energia['X_hat'], linewidth=1.2, color='red', label='Fitted')
plt.title('Consumo kWh/dia com valores ajustados', fontsize=16, fontweight='bold')
plt.xlabel('t', fontsize=14, style='italic')
plt.ylabel('Y_t', fontsize=14, style='italic')
plt.legend()
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.show()

# Out of sample performance
out_sample_mask = energia['X_hat'].notna()
mape = np.mean(np.abs((energia.loc[out_sample_mask, 'X_hat'] - 
                      energia.loc[out_sample_mask, 'Consumo']) / 
                      energia.loc[out_sample_mask, 'Consumo'])) * 100

print(f"\nOut of sample MAPE: {mape:.2f}%")

# ========================================================
# Forecasting
# ========================================================

# Forecast next 12 periods
forecast_steps = 12
forecast = full_fitted.get_forecast(steps=forecast_steps)
forecast_mean = forecast.predicted_mean
forecast_conf = forecast.conf_int()

# Create forecast dataframe
forecast_dates = pd.date_range(start=energia['mes'].iloc[-1] + pd.DateOffset(months=1), 
                              periods=forecast_steps, freq='M')

forecast_df = pd.DataFrame({
    'mes': forecast_dates,
    'X_hat': forecast_mean,
    'lower': forecast_conf.iloc[:, 0],
    'upper': forecast_conf.iloc[:, 1]
})

# Plot forecast
plt.figure(figsize=(12, 6))

# Plot historical data (line only)
plt.plot(energia['mes'].iloc[-24:], energia['Consumo'].iloc[-24:], 
         linewidth=1.2, color='black', label='Historical')

# Plot forecast
plt.plot(forecast_df['mes'], forecast_df['X_hat'], 
         linewidth=1.2, color='red', label='Forecast')

# Plot confidence intervals
plt.fill_between(forecast_df['mes'], forecast_df['lower'], forecast_df['upper'], 
                alpha=0.3, color='grey', label='95% Confidence Interval')

# Plot all data points (historical + forecast area) as black dots
# Historical points
plt.scatter(energia['mes'].iloc[-24:], energia['Consumo'].iloc[-24:], 
           color='black', s=30, zorder=5)

# If we have actual data for the forecast period, plot those points too
# (This would be the case if we're doing backtesting)
# For now, we'll just show the historical points

plt.title('Consumo kWh/dia (últimos 12 meses) com previsão', fontsize=16, fontweight='bold')
plt.xlabel('t', fontsize=14, style='italic')
plt.ylabel('Y_t', fontsize=14, style='italic')
plt.legend()
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.show()

print("\nForecast for next 12 periods:")
print(forecast_df[['mes', 'X_hat', 'lower', 'upper']].round(2))

# ========================================================
# Save results
# ========================================================

# Save the results to a CSV file
results_df = pd.DataFrame({
    'Date': energia['mes'],
    'Original': energia['Consumo'],
    'Fitted': energia['X_hat'],
    'Residuals': energia['X_res']
})

results_df.to_csv('sarima_results.csv', index=False)
print("\nResults saved to 'sarima_results.csv'")

# Save forecast results
forecast_df.to_csv('sarima_forecast.csv', index=False)
print("Forecast saved to 'sarima_forecast.csv'") 