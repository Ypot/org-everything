# Solución para Acentos en org-everything

## Problema
Los acentos (ñ, ó, á, etc.) se muestran incorrectamente en los resultados de búsqueda de Everything.

## Solución Rápida

### Opción 1: Automática (Recomendada)
1. Carga el archivo `fix-encoding.el` en Emacs:
   ```elisp
   (load-file "/ruta/a/fix-encoding.el")
   ```

2. Ejecuta el diagnóstico automático:
   ```
   M-x everything-fix-accents
   ```

3. El sistema probará automáticamente las codificaciones más comunes y te sugerirá la mejor.

### Opción 2: Manual
Si ya sabes qué codificación necesitas, edita la línea 48 en `org-everything.el`:

```elisp
;; Cambia esta línea por una de las siguientes:

;; Para Windows España (DOS):
(setq process-coding-system-alist '(("es" . (cp850 . utf-8))))

;; Para Windows Latin-1:
(setq process-coding-system-alist '(("es" . (cp1252 . utf-8))))

;; Para UTF-8:
(setq process-coding-system-alist '(("es" . (utf-8 . utf-8))))

;; Para ISO Latin-1:
(setq process-coding-system-alist '(("es" . (iso-8859-1 . utf-8))))
```

### Opción 3: Usar las funciones de debug del código original
El código incluye funciones avanzadas de diagnóstico:

1. **Diagnóstico rápido**:
   ```
   M-x org-everything-quick-test
   ```

2. **Análisis de bytes raw**:
   ```
   M-x org-everything-raw-bytes-test
   ```

3. **Prueba masiva de codificaciones**:
   ```
   M-x org-everything-massive-encoding-test
   ```

4. **Aplicar una codificación específica**:
   ```
   M-x org-everything-apply-encoding-fix
   ```

## Codificaciones más Comunes

| Codificación | Cuándo usar |
|-------------|-------------|
| `cp850` | Windows en español, codepage 850 (DOS) |
| `cp1252` | Windows Western, codepage 1252 |
| `iso-8859-1` | Latin-1, sistemas Unix/Linux |
| `iso-8859-15` | Latin-9, incluye símbolo € |
| `utf-8` | Sistemas modernos con UTF-8 |

## Verificación

Después de aplicar cualquier cambio:

1. Recarga el archivo `org-everything.el`:
   ```elisp
   (load-file "/ruta/a/org-everything.el")
   ```

2. Prueba la búsqueda:
   ```
   M-x org-everything
   ```

3. Busca algo que contenga acentos (ej: "producción", "diseño")

4. Verifica que los acentos se muestren correctamente

## Si Nada Funciona

1. Verifica que `es.exe` esté en tu PATH
2. Prueba ejecutar `es -r *` directamente en cmd/PowerShell
3. Verifica la codificación de tu terminal con `chcp` en Windows
4. Usa la función de diagnóstico completo:
   ```
   M-x org-everything-debug-complex
   ```

## Configuración ya Aplicada

El archivo `org-everything.el` ya ha sido modificado con:
- Configuración `cp850` como default (línea 48)
- Comentarios con opciones alternativas
- Todas las funciones de debug originales disponibles