import pandas as pd
import numpy as np

print("Checking data file...")

try:
    # Read the Excel file
    df = pd.read_excel('ConsumoEnergiaEAgua_New.xlsx')
    print(f"Shape: {df.shape}")
    print(f"Columns: {df.columns.tolist()}")
    print("\nFirst 5 rows:")
    print(df.head())
    print("\nData types:")
    print(df.dtypes)
    print("\nMissing values:")
    print(df.isnull().sum())
    
    # Check if we have the required columns
    if 'mes' in df.columns and 'Energia' in df.columns and 'Dias' in df.columns:
        print("\nProcessing data...")
        df['mes'] = pd.to_datetime(df['mes'])
        df['Consumo'] = df['Energia'] / df['Dias']
        
        # Only drop NaN from the columns we need
        df_clean = df[['mes', 'Consumo']].dropna()
        
        print(f"After processing - Shape: {df_clean.shape}")
        print(f"Consumo range: {df_clean['Consumo'].min():.2f} to {df_clean['Consumo'].max():.2f}")
        print(f"Consumo mean: {df_clean['Consumo'].mean():.2f}")
        
        # Calculate training size
        n_size = len(df_clean)
        t_size = int(3 * n_size / 4)
        X_t_train = df_clean['Consumo'].iloc[:t_size].values
        
        print(f"Training data length: {len(X_t_train)}")
        print(f"Training data range: {X_t_train.min():.2f} to {X_t_train.max():.2f}")
        
    else:
        print("Missing required columns!")
        
except Exception as e:
    print(f"Error: {e}") 