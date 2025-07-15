from flask import Flask, render_template, request
import pandas as pd
import os
from modelos_mixtos import aplicar_modelo
from juego import preguntas

app = Flask(__name__)

@app.route('/')
def index():
    return render_template('index.html')

@app.route('/aplicar', methods=['GET', 'POST'])
def aplicar():
    resultado = None
    tabla = None
    if request.method == 'POST':
        archivo = request.files['archivo']
        if archivo:
            if archivo.filename.endswith('.csv'):
                df = pd.read_csv(archivo)
            else:
                df = pd.read_excel(archivo)
            tabla = df.head().to_html(classes="table table-bordered", index=False)
            formula = request.form['formula']
            grupo = request.form['grupo']
            resultado = aplicar_modelo(df, formula, grupo)
    return render_template('aplicar_modelo.html', resultado=resultado, tabla=tabla)

@app.route('/aprender')
def aprender():
    return render_template('aprender_modelo.html')

@app.route('/practicar')
def practicar():
    return render_template('aplicar_conocimientos.html')
@app.route("/juego", methods=["GET", "POST"])
def juego():
    score = 0
    seleccionadas = []
    if request.method == "POST":
        for i, p in enumerate(preguntas):
            respuesta = request.form.get(f"preg_{i}")
            if respuesta == p["respuesta"]:
                score += 1
            seleccionadas.append(respuesta)
        return render_template("juego_resultado.html", score=score, total=len(preguntas))
    return render_template("juego.html", preguntas=preguntas)

if __name__ == '__main__':
    app.run(debug=True)
