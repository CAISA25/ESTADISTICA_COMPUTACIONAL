# Aplicación Educativa: Modelos Mixtos

Esta es una aplicación web educativa desarrollada con **Python** y **Flask**, enfocada en la enseñanza y aplicación de **modelos mixtos** en estadística. Está diseñada para ser fácil de usar por estudiantes, docentes e interesados en el análisis estadístico.

---

## 📁 Estructura del Proyecto

```
modelos_mixtos_app/
│
├── app.py                       # Archivo principal Flask
├── modelos_mixtos.py           # Lógica del modelo mixto
├── templates/                  # Plantillas HTML
│   ├── base.html
│   ├── index.html
│   ├── aplicar_modelo.html
│   ├── aprender_modelo.html
│   └── aplicar_conocimientos.html
├── static/
│   └── css/
│       └── estilos.css         # Estilo visual
├── uploads/                    # Para archivos importados
└── resultados/                 # Para archivos exportados
```

---

## 🚀 Cómo ejecutar

### 1. Instala los requisitos
Asegúrate de tener Python 3.7 o superior. Luego instala las dependencias:

```bash
pip install flask pandas statsmodels openpyxl
```

### 2. Ejecuta la aplicación

```bash
python app.py
```

Abre tu navegador en: [http://127.0.0.1:5000](http://127.0.0.1:5000)

---

## 🧪 Funcionalidades

### 🔹 Aplicar el Modelo
- Subida de archivos CSV o Excel
- Ingreso de fórmula y variable de agrupamiento
- Aplicación de modelo mixto (`MixedLM`)
- Resultados interpretables
- Exportación futura

### 🔹 Aprender el Modelo
- Sección educativa con explicaciones, ejemplos, videos, teoría

### 🔹 Aplicar Conocimientos
- Ejercicios interactivos para practicar

---

## 📌 Notas
- Esta es una versión base. Puedes mejorar la interfaz, agregar autenticación o exportación de resultados.
- Para modificar estilos, edita `static/css/estilos.css`.

---

**Desarrollado por:** Nelson Catunta Huisa  
**Curso:** Estadística Computacional  
