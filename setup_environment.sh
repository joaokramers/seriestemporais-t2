#!/bin/bash

echo "========================================"
echo "Configurando ambiente virtual para SARIMA"
echo "========================================"

echo ""
echo "1. Criando ambiente virtual..."
python3 -m venv sarima_env

echo ""
echo "2. Ativando ambiente virtual..."
source sarima_env/bin/activate

echo ""
echo "3. Atualizando pip..."
python -m pip install --upgrade pip

echo ""
echo "4. Instalando dependencias..."
pip install -r requirements.txt

echo ""
echo "========================================"
echo "Ambiente configurado com sucesso!"
echo "========================================"
echo ""
echo "Para ativar o ambiente virtual:"
echo "source sarima_env/bin/activate"
echo ""
echo "Para executar a analise:"
echo "python sarima_analysis.py"
echo "" 