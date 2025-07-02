@echo off
echo ========================================
echo Configurando ambiente virtual para SARIMA
echo ========================================

echo.
echo 1. Criando ambiente virtual...
python -m venv sarima_env

echo.
echo 2. Ativando ambiente virtual...
call sarima_env\Scripts\activate.bat

echo.
echo 3. Atualizando pip...
python -m pip install --upgrade pip

echo.
echo 4. Instalando dependencias...
pip install -r requirements.txt

echo.
echo ========================================
echo Ambiente configurado com sucesso!
echo ========================================
echo.
echo Para ativar o ambiente virtual:
echo sarima_env\Scripts\activate.bat
echo.
echo Para executar a analise:
echo python sarima_analysis.py
echo.
pause 