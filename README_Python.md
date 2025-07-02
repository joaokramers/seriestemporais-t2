# SARIMA Analysis - Python Version

This is a Python conversion of the original R code for time series analysis using SARIMA models for household electricity consumption data.

## Setup - Ambiente Virtual (Recomendado)

### Opção 1: Script Automático

**Windows:**
```bash
setup_environment.bat
```

**Linux/Mac:**
```bash
chmod +x setup_environment.sh
./setup_environment.sh
```

### Opção 2: Manual

1. **Criar ambiente virtual:**
```bash
python -m venv sarima_env
```

2. **Ativar ambiente virtual:**

   **Windows (PowerShell):**
   ```bash
   sarima_env\Scripts\Activate.ps1
   ```

   **Windows (Command Prompt):**
   ```bash
   sarima_env\Scripts\activate.bat
   ```

   **Linux/Mac:**
   ```bash
   source sarima_env/bin/activate
   ```

3. **Instalar dependências:**
```bash
pip install -r requirements.txt
```

### Desativar ambiente virtual:
```bash
deactivate
```

## Requirements (Sem ambiente virtual)

Install the required packages using:

```bash
pip install -r requirements.txt
```

## Data Format

The script expects an Excel file named `ConsumoEnergiaEAgua_New.xlsx` with the following columns:
- `mes`: Date column
- `Energia`: Energy consumption
- `Dias`: Number of days

## Usage

Run the analysis:

```bash
python sarima_analysis.py
```

## What the script does

1. **Data Loading**: Reads the Excel file and calculates daily consumption
2. **Exploratory Analysis**: 
   - Plots the time series
   - Calculates and plots ACF and PACF
   - Performs Augmented Dickey-Fuller test for stationarity
3. **Differencing**: Creates first differences and analyzes them
4. **Model Selection**: Performs grid search to find the best SARIMA model parameters
5. **Model Fitting**: Fits the best SARIMA model
6. **Diagnostics**: 
   - Ljung-Box test for residual autocorrelation
   - Residual ACF and PACF plots
   - Shapiro-Wilk test for normality
7. **Validation**: Out-of-sample validation with MAPE calculation
8. **Forecasting**: Generates 12-period forecasts with confidence intervals
9. **Results**: Saves results to CSV files

## Output Files

- `sarima_results.csv`: Original data, fitted values, and residuals
- `sarima_forecast.csv`: Forecast values with confidence intervals

## Key Differences from R Version

- Uses `statsmodels` instead of `TSA` package
- Uses `SARIMAX` class for model fitting
- Grid search is implemented manually since Python doesn't have a direct equivalent to R's `sarima` function
- Plotting uses `matplotlib` and `seaborn` instead of `ggplot2`
- Results are saved in CSV format instead of R's native format

## BIC Differences

**Important Note**: The BIC values in Python may differ from R due to:

1. **Different optimization algorithms**: R's `TSA` package uses different optimization methods than `statsmodels`
2. **Different convergence criteria**: The stopping criteria for model fitting may differ
3. **Different parameter constraints**: How stationarity and invertibility are enforced
4. **Different initial values**: Starting points for parameter estimation

### Files for BIC Analysis:

- `sarima_analysis.py`: Original version with basic BIC calculation
- `sarima_analysis_improved.py`: Improved version with R-like BIC calculation
- `compare_bic.py`: Script to compare BIC values between R and Python

### Running BIC Comparison:

```bash
python compare_bic.py
```

This will show you the differences between R and Python BIC values for the top models.

## Notes

- The script automatically handles missing values and data type conversions
- All plots are displayed interactively and can be saved manually
- The grid search may take some time depending on the data size and parameter ranges 