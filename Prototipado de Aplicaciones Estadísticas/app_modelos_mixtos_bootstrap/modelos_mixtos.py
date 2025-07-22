from statsmodels.regression.mixed_linear_model import MixedLM

def aplicar_modelo(df, formula, grupo):
    # Normalizar el nombre del grupo
    grupo_col = next((col for col in df.columns if col.strip().lower() == grupo.strip().lower()), None)
    if not grupo_col:
        return f"❌ Error: No se encontró la columna '{grupo}'. Columnas disponibles: {', '.join(df.columns)}"

    # Convertir a tipo categórico
    df[grupo_col] = df[grupo_col].astype('category')

    try:
        modelo = MixedLM.from_formula(formula, groups=df[grupo_col], data=df)
        resultado = modelo.fit()
        return resultado.summary().as_text()
    except Exception as e:
        return f"⚠️ Error al ajustar el modelo:\n{e}"
