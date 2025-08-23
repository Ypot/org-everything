;;; org-everything.el --- Everything integration with Consult -*- lexical-binding: t; -*-

;; Copyright (C) John Haman 2022, Modified by Ypot 2025
;; Original Author: John Haman <mail@johnhaman.org>
;; Modified by: Ypot <ypo@foro.aleeas.com>
;; Original Homepage: https://github.com/jthaman/consult-everything
;; Package-Requires: ((emacs "25.1") (consult "0.15"))
;; Version: 0.2.1

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides Everything integration with the Emacs program
;; `consult'. Download the Everything command line tool, es.exe, from
;; https://www.voidtools.com/downloads/#cli.
;;
;; Everything is a useful `locate' alternative on Windows machines.
;;
;; This is a fork of consult-everything with improved encoding support
;; for Windows systems, particularly for handling accented characters
;; and special symbols in file names.

;;; License:

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE. See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'consult)

;; Configuración de codificación para Everything
(setq process-coding-system-alist
      '(("es" . (windows-1252 . windows-1252))))

;; Función de debug automático para probar codificaciones
(defun org-everything--test-search (query)
  "Test search with current encoding configuration."
  (let ((results '())
        (temp-buffer (get-buffer-create "*Everything-Test*")))
    (with-current-buffer temp-buffer
      (erase-buffer)
      (let ((process (start-process "es-test" temp-buffer "es" "-r" query)))
        (while (process-live-p process)
          (accept-process-output process 0.1))
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
            (unless (string-blank-p line)
              (push line results)))
          (forward-line 1))))
    (kill-buffer temp-buffer)
    results))

;; Sistema de debug complejo para analizar el flujo completo
(defun org-everything-debug-complex ()
  "Debug complejo que analiza cada paso del proceso de codificación."
  (interactive)
  (let ((debug-buffer (get-buffer-create "*Everything-Complex-Debug*"))
        (test-query "producción")
        (current-encoding (assoc "es" process-coding-system-alist)))
    
    (with-current-buffer debug-buffer
      (erase-buffer)
      (insert "=== EVERYTHING COMPLEX DEBUG ===\n\n")
      
      ;; 1. Análisis del sistema
      (insert "1. ANÁLISIS DEL SISTEMA:\n")
      (insert (format "   - Sistema operativo: %s\n" system-type))
      (insert (format "   - Versión de Emacs: %s\n" emacs-version))
      (insert (format "   - Codificación del buffer: %s\n" buffer-file-coding-system))
      (insert (format "   - Codificación por defecto: %s\n" default-file-name-coding-system))
      (insert (format "   - Codificación del terminal: %s\n" terminal-coding-system))
      (insert (format "   - Codificación del sistema: %s\n" system-file-name-coding-system))
      (insert "\n")
      
      ;; 2. Análisis de la configuración actual
      (insert "2. CONFIGURACIÓN ACTUAL:\n")
      (insert (format "   - process-coding-system-alist: %s\n" process-coding-system-alist))
      (insert (format "   - default-process-coding-system: %s\n" default-process-coding-system))
      (insert "\n")
      
      ;; 3. Prueba con configuración actual
      (insert "3. PRUEBA CON CONFIGURACIÓN ACTUAL:\n")
      (insert (format "   - Configuración: %s\n" current-encoding))
      (insert (format "   - Query de prueba: '%s'\n" test-query))
      
      ;; Ejecutar búsqueda y analizar resultados
      (let ((search-results (org-everything--test-search test-query)))
        (insert (format "   - Resultados encontrados: %d\n" (length search-results)))
        (insert "   - Primeros 3 resultados:\n")
        (dolist (result (seq-take search-results 3))
          (insert (format "     '%s'\n" result)))
        (insert "\n")
        
        ;; 4. Análisis de codificación de resultados
        (insert "4. ANÁLISIS DE CODIFICACIÓN DE RESULTADOS:\n")
        (dolist (result (seq-take search-results 3))
          (insert (format "   Resultado: '%s'\n" result))
          (insert (format "   - Longitud: %d\n" (length result)))
          (insert (format "   - Bytes: %s\n" (string-to-list result)))
          (insert (format "   - Codificación detectada: %s\n" 
                         (org-everything--detect-encoding result)))
          (insert "\n"))
        
        ;; 5. Prueba de conversión de codificación
        (insert "5. PRUEBA DE CONVERSIÓN DE CODIFICACIÓN:\n")
        (let ((sample-result (car search-results)))
          (when sample-result
            (insert (format "   Resultado de muestra: '%s'\n" sample-result))
            (insert "   Conversiones:\n")
            (dolist (target-encoding '(utf-8 windows-1252 cp1252 iso-8859-1))
              (condition-case err
                  (let ((converted (decode-coding-string sample-result target-encoding)))
                    (insert (format "     %s -> '%s'\n" target-encoding converted)))
                (error
                 (insert (format "     %s -> ERROR: %s\n" target-encoding err))))))
        (insert "\n")
        
        ;; 6. Análisis del proceso externo
        (insert "6. ANÁLISIS DEL PROCESO EXTERNO:\n")
        (insert "   - Comando ejecutado: es -r 'producción'\n")
        (insert "   - Proceso: es-test\n")
        (insert "   - Buffer temporal: *Everything-Test*\n")
        (insert "\n")
        
        ;; 7. Recomendaciones
        (insert "7. RECOMENDACIONES:\n")
        (insert "   Basado en el análisis:\n")
        (if (> (length search-results) 1)
            (insert "   ✓ La búsqueda funciona (más de 1 resultado)\n")
          (insert "   ✗ La búsqueda falla (solo 1 resultado)\n"))
        (insert "   - Verificar si los caracteres se muestran correctamente\n")
        (insert "   - Probar diferentes codificaciones de recepción\n")
        (insert "   - Considerar usar consult--async-command con codificación específica\n")
        (insert "\n")
        
        ;; 8. Próximos pasos
        (insert "8. PRÓXIMOS PASOS:\n")
        (insert "   - Ejecutar org-everything con la configuración actual\n")
        (insert "   - Verificar visualización de resultados\n")
        (insert "   - Probar configuración (utf-8 . windows-1252)\n")
        (insert "   - Analizar diferencias entre búsqueda directa y Consult\n")
        (insert "\n")
        
        (insert "=== DEBUG COMPLETO ===\n"))
      
      (display-buffer debug-buffer))))

;; Función auxiliar para detectar codificación
(defun org-everything--detect-encoding (string)
  "Detect encoding of a string based on byte patterns."
  (let ((bytes (string-to-list string))
        (has-high-bytes nil)
        (has-utf8-pattern nil))
    
    ;; Detectar bytes altos (caracteres especiales)
    (dolist (byte bytes)
      (when (> byte 127)
        (setq has-high-bytes t)))
    
    ;; Detectar patrones UTF-8
    (let ((i 0))
      (while (< i (length bytes))
        (let ((byte (nth i bytes)))
          (when (and (> byte 127) (< i (1- (length bytes))))
            (let ((next-byte (nth (1+ i) bytes)))
              (when (and next-byte (>= next-byte 128))
                (setq has-utf8-pattern t))))
          (setq i (1+ i)))))
    
    ;; Determinar codificación más probable
    (cond
     ((and has-high-bytes (not has-utf8-pattern))
      "windows-1252/cp1252 (probable)")
     (has-utf8-pattern
      "UTF-8 (probable)")
     (has-high-bytes
      "Codificación de 8 bits (no UTF-8)")
           (t
       "ASCII"))))

;; ===== SISTEMA DE DEBUG SUPERIOR PARA CODIFICACIONES =====
;; Basado en análisis específico de códigos problemáticos

(defun org-everything-debug-encodings ()
  "Debug encoding issues by testing different codifications with problematic characters."
  (interactive)
  (let* ((test-codes '(?\242 ?\340)) ; Los códigos problemáticos que reportaste
         (encodings '(cp850 cp437 cp1252 windows-1252 iso-8859-1 latin-1 utf-8 
                      cp866 cp852 cp857 cp860 cp861 cp863 cp865 cp869
                      mac-roman mac-cyrillic mac-greek mac-turkish
                      iso-8859-2 iso-8859-3 iso-8859-4 iso-8859-5 iso-8859-6 
                      iso-8859-7 iso-8859-8 iso-8859-9 iso-8859-10 iso-8859-13
                      iso-8859-14 iso-8859-15 iso-8859-16
                      koi8-r koi8-u big5 gbk gb18030 euc-jp euc-kr
                      shift-jis jis-7 jis-8))
         (buffer-name "*Encoding Debug*"))
    
    ;; Crear buffer de debug
    (with-output-to-temp-buffer buffer-name
      (princ "=== DEBUG DE CODIFICACIONES PARA ORG-EVERYTHING ===\n\n")
      (princ (format "Códigos problemáticos: \\242 (octal %o, decimal %d) y \\340 (octal %o, decimal %d)\n\n" 
                     ?\242 ?\242 ?\340 ?\340))
      
      ;; Probar cada codificación
      (dolist (encoding encodings)
        (princ (format "--- Codificación: %s ---\n" encoding))
        (condition-case err
            (progn
              (princ (format "  \\242 → '%s'\n" 
                           (decode-coding-string (string ?\242) encoding)))
              (princ (format "  \\340 → '%s'\n" 
                           (decode-coding-string (string ?\340) encoding))))
          (error 
           (princ (format "  ERROR: %s\n" (error-message-string err)))))
        (princ "\n"))
      
      ;; Información adicional
      (princ "=== INFORMACIÓN DEL SISTEMA ===\n")
      (princ (format "system-type: %s\n" system-type))
      (princ (format "locale-coding-system: %s\n" locale-coding-system))
      (princ (format "default-process-coding-system: %s\n" default-process-coding-system))
      (princ (format "file-name-coding-system: %s\n" file-name-coding-system))
      
      (princ "\n=== PRUEBA MANUAL ===\n")
      (princ "Para probar manualmente una codificación, usa:\n")
      (princ "(decode-coding-string \"\\242\\340\" 'CODIFICACION)\n")
      (princ "\nEjemplo:\n")
      (princ "(decode-coding-string \"\\242\\340\" 'cp850)\n"))))

(defun org-everything-test-encoding (encoding)
  "Test a specific ENCODING with the problematic characters."
  (interactive 
   (list (intern (completing-read "Encoding: " 
                                  '("cp850" "cp437" "cp1252" "windows-1252" 
                                    "iso-8859-1" "latin-1" "utf-8" "cp866" "cp852"
                                    "mac-roman" "iso-8859-15" "koi8-r" "big5")))))
  (message "\\242 → '%s', \\340 → '%s' (usando %s)" 
           (decode-coding-string (string ?\242) encoding)
           (decode-coding-string (string ?\340) encoding)
           encoding))

(defun org-everything-apply-encoding-fix (encoding)
  "Apply the encoding fix using the specified ENCODING."
  (interactive 
   (list (intern (completing-read "Encoding to apply: " 
                                  '("cp850" "cp437" "cp1252" "windows-1252" 
                                    "iso-8859-1" "latin-1" "cp866" "cp852" "mac-roman"
                                    "iso-8859-15" "koi8-r" "big5")))))
  
  ;; Remover configuración previa si existe
  (setq process-coding-system-alist 
        (assq-delete-all "es" process-coding-system-alist))
  
  ;; Aplicar nueva configuración
  (add-to-list 'process-coding-system-alist `("es" . (,encoding . utf-8)))
  
  (message "Aplicada codificación %s para 'es'. Prueba org-everything ahora." encoding))

;; Función alternativa para probar en tiempo real
(defun org-everything-live-encoding-test ()
  "Interactive test to find the correct encoding by trying them one by one."
  (interactive)
  (let ((encodings '(cp850 cp437 cp1252 windows-1252 iso-8859-1 latin-1 cp866 cp852
                      mac-roman iso-8859-15 koi8-r big5))
        (test-string "\242\340") ; Producción → Producci\242n, Ó → \340
        (found nil))
    
    (dolist (encoding encodings)
      (unless found
        (let ((decoded (decode-coding-string test-string encoding)))
          (when (y-or-n-p (format "¿Es correcto '%s' usando %s? " decoded encoding))
            (org-everything-apply-encoding-fix encoding)
            (setq found t)))))
    
    (unless found
      (message "No se encontró la codificación correcta. Prueba manualmente."))))

(defun org-everything-test-real-encoding ()
  "Test real encoding by calling es.exe directly with different coding systems."
  (interactive)
  (let ((test-query "producción") ; buscar algo que contenga ñ y ó
        (encodings '(cp850 cp437 cp1252 windows-1252 iso-8859-1 latin-1 raw-text
                      cp866 cp852 cp857 cp860 cp861 cp863 cp865 cp869
                      mac-roman mac-cyrillic mac-greek mac-turkish
                      iso-8859-2 iso-8859-3 iso-8859-4 iso-8859-5 iso-8859-6 
                      iso-8859-7 iso-8859-8 iso-8859-9 iso-8859-10 iso-8859-13
                      iso-8859-14 iso-8859-15 iso-8859-16
                      koi8-r koi8-u big5 gbk gb18030 euc-jp euc-kr
                      shift-jis jis-7 jis-8))
        (buffer-name "*Real Encoding Test*"))
    
    (with-output-to-temp-buffer buffer-name
      (princ "=== PRUEBA REAL CON ES.EXE ===\n\n")
      (princ (format "Buscando: %s\n" test-query))
      (princ "Si no tienes archivos con ese nombre, usa cualquier búsqueda que devuelva archivos con ñ/ó\n\n")
      
      (dolist (encoding encodings)
        (princ (format "--- Probando codificación: %s ---\n" encoding))
        (condition-case err
            (let* ((process-coding-system-alist `(("es" . (,encoding . utf-8))))
                   (default-process-coding-system `(,encoding . utf-8))
                   (cmd (split-string org-everything-args))
                   (full-cmd (append cmd (list test-query)))
                   (result (with-temp-buffer
                            (apply #'call-process (car full-cmd) nil t nil (cdr full-cmd))
                            (buffer-string))))
              (princ (format "Resultado (primeras 300 chars):\n%s\n" 
                           (substring result 0 (min 300 (length result)))))
              (princ (format "Contiene caracteres problemáticos: %s\n"
                           (if (or (string-match "\\\\[0-9]+" result)
                                   (string-match "¢\\|à" result))
                               "SÍ" "NO"))))
          (error 
           (princ (format "ERROR: %s\n" (error-message-string err)))))
        (princ "\n"))
      
      (princ "=== INSTRUCCIONES ===\n")
      (princ "1. Busca la codificación donde NO aparezcan \\nnn ni ¢/à\n")
      (princ "2. Esa será la codificación correcta\n")
      (princ "3. Aplícala con: M-x org-everything-apply-encoding-fix\n"))))

(defun org-everything-raw-bytes-test ()
  "Show raw bytes from es.exe output to understand the real encoding."
  (interactive)
  (let* ((test-query (read-string "Buscar (algo que tenga ñ/ó, o * para todo): " "*"))
         (cmd (split-string org-everything-args))
         (full-cmd (append cmd (list test-query)))
         (buffer-name "*Raw Bytes Test*"))
    
    (with-output-to-temp-buffer buffer-name
      (princ "=== ANÁLISIS DE BYTES RAW ===\n\n")
      (princ (format "Comando: %s\n\n" (mapconcat 'identity full-cmd " ")))
      
      ;; Capturar con raw-text para ver los bytes reales
      (let* ((process-coding-system-alist '(("es" . (raw-text . raw-text))))
             (result (with-temp-buffer
                      (apply #'call-process (car full-cmd) nil t nil (cdr full-cmd))
                      (buffer-string))))
        
        (princ "Primeros 1000 caracteres en RAW:\n")
        (princ (substring result 0 (min 1000 (length result))))
        (princ "\n\n")
        
        (if (string-empty-p (string-trim result))
            (progn
              (princ "¡NO HAY RESULTADOS!\n")
              (princ "Prueba con:\n")
              (princ "- Una búsqueda más simple como '*' o '*.txt'\n")
              (princ "- Verifica que es.exe esté en el PATH\n")
              (princ "- Ejecuta manualmente: es -r * en cmd\n"))
          (progn
            (princ "Análisis byte por byte de caracteres problemáticos:\n")
            (let ((pos 0)
                  (found-problem-chars nil))
              (while (and (< pos (length result)) (< pos 2000)) ; limitar a 2000 chars
                (let ((char (aref result pos)))
                  (when (or (= char 162) (= char 224) ; 242 octal = 162 decimal, 340 octal = 224 decimal
                           (and (> char 127) (< char 256))) ; cualquier carácter no-ASCII
                    (princ (format "Posición %d: byte %d (octal \\%o, hex \\x%x) → char '%c'\n" 
                                 pos char char char char))
                    (setq found-problem-chars t)))
                (setq pos (1+ pos)))
              (unless found-problem-chars
                (princ "No se encontraron caracteres problemáticos en los primeros 2000 bytes.\n")
                (princ "Esto podría significar:\n")
                (princ "1. Los archivos no tienen caracteres especiales\n")
                (princ "2. es.exe está devolviendo UTF-8 correctamente\n")
                (princ "3. El problema está en otro lugar\n")))))))))

(defun org-everything-quick-test ()
  "Quick test to see if es.exe works and what encoding it uses."
  (interactive)
  (let ((buffer-name "*Quick ES Test*"))
    (with-output-to-temp-buffer buffer-name
      (princ "=== PRUEBA RÁPIDA DE ES.EXE ===\n\n")
      
      ;; Test 1: Ver si es.exe funciona
      (princ "1. Probando si es.exe funciona con búsqueda simple...\n")
      (condition-case err
          (let* ((result (shell-command-to-string "es -r *.txt | head -5")))
            (if (string-empty-p (string-trim result))
                (princ "   No hay resultados para *.txt\n")
              (princ (format "   Resultados encontrados:\n%s\n" result))))
        (error (princ (format "   ERROR: %s\n" (error-message-string err)))))
      
      ;; Test 2: Buscar cualquier cosa
      (princ "\n2. Probando búsqueda más amplia...\n")
      (condition-case err
          (let* ((result (shell-command-to-string "es -r -n 10 *")))
            (if (string-empty-p (string-trim result))
                (princ "   No hay resultados para *\n")
              (princ (format "   Primeros resultados:\n%s\n" 
                           (substring result 0 (min 500 (length result)))))))
        (error (princ (format "   ERROR: %s\n" (error-message-string err)))))
      
      ;; Test 3: Info del sistema
      (princ "\n3. Información del sistema:\n")
      (princ (format "   LANG: %s\n" (getenv "LANG")))
      (princ (format "   LC_ALL: %s\n" (getenv "LC_ALL")))
      (princ (format "   Codepage (chcp): "))
      (condition-case err
          (princ (string-trim (shell-command-to-string "chcp")))
        (error (princ "No disponible")))
      (princ "\n")
      
      (princ "\n=== INSTRUCCIONES ===\n")
      (princ "Si ves archivos listados arriba:\n")
      (princ "1. Busca alguno que tenga ñ, ó, u otros acentos\n")
      (princ "2. Ejecuta: M-x org-everything-raw-bytes-test\n")
      (princ "3. Busca ese archivo específico\n")
      (princ "\nSi no ves archivos:\n")
      (princ "1. Verifica que es.exe esté instalado\n")
      (princ "2. Verifica que esté en el PATH\n")
      (princ "3. Prueba ejecutar 'es -r *' manualmente en cmd\n"))))

(defun org-everything-test-specific-files ()
  "Test encoding with the specific files we found."
  (interactive)
  (let ((test-queries '("Diseño" "Catálogos" "dise" "cat" "producción" "fabricación"))
        (encodings '(cp1252 windows-1252 cp850 iso-8859-1 latin-1 raw-text
                      cp866 cp852 cp857 cp860 cp861 cp863 cp865 cp869
                      mac-roman mac-cyrillic mac-greek mac-turkish
                      iso-8859-2 iso-8859-3 iso-8859-4 iso-8859-5 iso-8859-6 
                      iso-8859-7 iso-8859-8 iso-8859-9 iso-8859-10 iso-8859-13
                      iso-8859-14 iso-8859-15 iso-8859-16
                      koi8-r koi8-u big5 gbk gb18030 euc-jp euc-kr
                      shift-jis jis-7 jis-8))
        (buffer-name "*Specific Files Test*"))
    
    (with-output-to-temp-buffer buffer-name
      (princ "=== PRUEBA CON ARCHIVOS ESPECÍFICOS ===\n\n")
      
      (dolist (query test-queries)
        (princ (format "Probando búsqueda: %s\n" query))
        (princ "----------------------------------------\n")
        
        (dolist (encoding encodings)
          (princ (format "Codificación %s: " encoding))
          (condition-case err
              (let* ((process-coding-system-alist `(("es" . (,encoding . utf-8))))
                     (cmd (split-string org-everything-args))
                     (full-cmd (append cmd (list query)))
                     (result (with-temp-buffer
                              (apply #'call-process (car full-cmd) nil t nil (cdr full-cmd))
                              (buffer-string))))
                (if (string-empty-p (string-trim result))
                    (princ "Sin resultados\n")
                  (let ((first-line (car (split-string result "\n"))))
                    (princ (format "%s\n" 
                                 (if (> (length first-line) 80)
                                     (concat (substring first-line 0 77) "...")
                                   first-line))))))
            (error (princ (format "ERROR: %s\n" (error-message-string err))))))
        
        (princ "\n")))
    
    ;; Mostrar también los bytes RAW de uno de estos archivos
    (princ "\n=== ANÁLISIS RAW DEL PRIMER ARCHIVO ===\n")
    (let* ((process-coding-system-alist '(("es" . (raw-text . raw-text))))
           (cmd (split-string org-everything-args))
           (full-cmd (append cmd '("dise")))
           (result (with-temp-buffer
                    (apply #'call-process (car full-cmd) nil t nil (cdr full-cmd))
                    (buffer-string))))
      (when (not (string-empty-p (string-trim result)))
        (let ((first-line (car (split-string result "\n"))))
          (princ (format "Línea RAW: %s\n\n" first-line))
          (princ "Análisis byte por byte:\n")
          (let ((pos 0))
            (while (< pos (min (length first-line) 200))
              (let ((char (aref first-line pos)))
                (when (> char 127)
                  (princ (format "Posición %d: byte %d (\\%o octal) → '%c'\n" 
                               pos char char char))))
              (setq pos (1+ pos)))))))))

;; ===== FUNCIÓN DE PRUEBA MÁSIVA EXPANDIDA =====
;; Combina tu enfoque científico con mi enfoque de fuerza bruta

(defun org-everything-massive-encoding-test ()
  "Massive test of ALL possible encoding combinations for maximum coverage."
  (interactive)
  (let ((test-query "producción")
        (encodings '(
          ;; Codificaciones DOS/Windows (las más probables)
          cp437 cp850 cp851 cp852 cp853 cp855 cp856 cp857 cp858 cp859 cp860 cp861 cp862 cp863 cp864 cp865 cp866 cp867 cp868 cp869 cp874 cp932 cp936 cp949 cp950 cp1250 cp1251 cp1252 cp1253 cp1254 cp1255 cp1256 cp1257 cp1258
          
          ;; Codificaciones ISO (estándar internacional)
          iso-8859-1 iso-8859-2 iso-8859-3 iso-8859-4 iso-8859-5 iso-8859-6 iso-8859-7 iso-8859-8 iso-8859-9 iso-8859-10 iso-8859-13 iso-8859-14 iso-8859-15 iso-8859-16
          
          ;; Codificaciones Mac (sistemas Apple)
          mac-roman mac-cyrillic mac-greek mac-turkish mac-icelandic mac-ce mac-romanian mac-ukrainian mac-thai mac-croatian mac-gaelic mac-icelandic mac-romanian mac-ukrainian
          
          ;; Codificaciones Unix/Linux
          koi8-r koi8-u koi8-ru koi8-t
          
          ;; Codificaciones Asiáticas
          big5 big5-hkscs gbk gb18030 euc-jp euc-kr euc-tw shift-jis jis-7 jis-8
          
          ;; Codificaciones especiales
          raw-text undecided nil
        ))
        (results '())
        (original-encoding (assoc "es" process-coding-system-alist))
        (buffer-name "*Massive Encoding Test*"))
    
    (with-output-to-temp-buffer buffer-name
      (princ "=== PRUEBA MÁSIVA DE CODIFICACIONES ===\n\n")
      (princ (format "Probando %d codificaciones con query: '%s'\n\n" (length encodings) test-query))
      
      (dolist (encoding encodings)
        (princ (format "Probando: %s" encoding))
        (condition-case err
            (let* ((process-coding-system-alist `(("es" . (,encoding . utf-8))))
                   (search-results (org-everything--test-search test-query)))
              (princ (format " → %d resultados\n" (length search-results)))
              (push (cons encoding search-results) results))
          (error 
           (princ (format " → ERROR: %s\n" (error-message-string err)))
           (push (cons encoding '()) results))))
      
      ;; Restaurar configuración original
      (setq process-coding-system-alist (list original-encoding))
      
      ;; Mostrar resumen
      (princ "\n=== RESUMEN DE RESULTADOS ===\n")
      (let ((working-encodings '())
            (non-working-encodings '()))
        (dolist (result results)
          (if (> (length (cdr result)) 1)
              (push (car result) working-encodings)
            (push (car result) non-working-encodings)))
        
        (princ (format "Codificaciones FUNCIONAN (%d): %s\n" 
                      (length working-encodings) working-encodings))
        (princ (format "Codificaciones NO FUNCIONAN (%d): %s\n" 
                      (length non-working-encodings) non-working-encodings))
        
        (princ "\n=== RECOMENDACIONES ===\n")
        (princ "1. Las codificaciones que FUNCIONAN son candidatas para la solución\n")
        (princ "2. Prueba cada una con: M-x org-everything-test-encoding\n")
        (princ "3. Aplica la mejor con: M-x org-everything-apply-encoding-fix\n")
        (princ "4. Para análisis detallado: M-x org-everything-raw-bytes-test\n")))))

;; ===== USO RECOMENDADO =====
;; 1. M-x org-everything-test-specific-files    ; ¡PRUEBA ESTA PRIMERA!
;; 2. M-x org-everything-quick-test             ; Prueba rápida inicial  
;; 3. M-x org-everything-raw-bytes-test         ; Ver bytes RAW 
;; 4. M-x org-everything-test-real-encoding     ; Prueba REAL con es.exe
;; 5. M-x org-everything-massive-encoding-test  ; Prueba MÁSIVA de todas
;; 6. M-x org-everything-apply-encoding-fix     ; Aplicar la correcta

(defun org-everything-test-encodings ()
  "Test multiple encoding configurations automatically."
  (interactive)
  (let ((test-query "producción")
        (encodings '(
          ;; Codificaciones básicas
          (utf-8 . utf-8)
          (windows-1252 . windows-1252)
          (windows-1252 . utf-8)
          (utf-8 . windows-1252)
          (undecided . undecided)
          (nil . nil)
          (cp1252 . cp1252)
          (iso-8859-1 . iso-8859-1)
          
          ;; Variaciones de Windows
          (cp1252 . cp1252)
          (cp1252 . utf-8)
          (utf-8 . cp1252)
          (windows-1252 . cp1252)
          (cp1252 . windows-1252)
          
          ;; Variaciones de ISO
          (iso-8859-1 . iso-8859-1)
          (iso-8859-1 . utf-8)
          (utf-8 . iso-8859-1)
          (iso-8859-1 . windows-1252)
          (windows-1252 . iso-8859-1)
          
          ;; Codificaciones específicas de Windows
          (windows-1250 . windows-1250)
          (windows-1250 . utf-8)
          (utf-8 . windows-1250)
          (windows-1251 . windows-1251)
          (windows-1251 . utf-8)
          (utf-8 . windows-1251)
          (windows-1253 . windows-1253)
          (windows-1253 . utf-8)
          (utf-8 . windows-1253)
          (windows-1254 . windows-1254)
          (windows-1254 . utf-8)
          (utf-8 . windows-1254)
          (windows-1255 . windows-1255)
          (windows-1255 . utf-8)
          (utf-8 . windows-1255)
          (windows-1256 . windows-1256)
          (windows-1256 . utf-8)
          (utf-8 . windows-1256)
          (windows-1257 . windows-1257)
          (windows-1257 . utf-8)
          (utf-8 . windows-1257)
          (windows-1258 . windows-1258)
          (windows-1258 . utf-8)
          (utf-8 . windows-1258)
          
          ;; Variaciones de UTF
          (utf-8-unix . utf-8-unix)
          (utf-8-unix . utf-8)
          (utf-8 . utf-8-unix)
          (utf-8-dos . utf-8-dos)
          (utf-8-dos . utf-8)
          (utf-8 . utf-8-dos)
          (utf-8-mac . utf-8-mac)
          (utf-8-mac . utf-8)
          (utf-8 . utf-8-mac)
          
          ;; Codificaciones de sistema
          (system . system)
          (system . utf-8)
          (utf-8 . system)
          (system . windows-1252)
          (windows-1252 . system)
          
          ;; Combinaciones mixtas
          (undecided . utf-8)
          (utf-8 . undecided)
          (undecided . windows-1252)
          (windows-1252 . undecided)
          (undecided . cp1252)
          (cp1252 . undecided)
          
          ;; Codificaciones específicas para español
          (iso-8859-15 . iso-8859-15)
          (iso-8859-15 . utf-8)
          (utf-8 . iso-8859-15)
          (iso-8859-15 . windows-1252)
          (windows-1252 . iso-8859-15)
          
          ;; Codificaciones de IBM
          (ibm437 . ibm437)
          (ibm437 . utf-8)
          (utf-8 . ibm437)
          (ibm437 . windows-1252)
          (windows-1252 . ibm437)
          
          ;; Codificaciones de Mac
          (mac-roman . mac-roman)
          (mac-roman . utf-8)
          (utf-8 . mac-roman)
          (mac-roman . windows-1252)
          (windows-1252 . mac-roman)
          
          ;; Codificaciones de Unix
          (unix . unix)
          (unix . utf-8)
          (utf-8 . unix)
          (unix . windows-1252)
          (windows-1252 . unix)
          
          ;; Codificaciones de DOS
          (dos . dos)
          (dos . utf-8)
          (utf-8 . dos)
          (dos . windows-1252)
          (windows-1252 . dos)
          
          ;; Combinaciones extremas
          (nil . utf-8)
          (utf-8 . nil)
          (nil . windows-1252)
          (windows-1252 . nil)
          (nil . cp1252)
          (cp1252 . nil)
          
          ;; Codificaciones específicas de Emacs
          (emacs-mule . emacs-mule)
          (emacs-mule . utf-8)
          (utf-8 . emacs-mule)
          (emacs-mule . windows-1252)
          (windows-1252 . emacs-mule)
          
          ;; Codificaciones de terminal
          (terminal . terminal)
          (terminal . utf-8)
          (utf-8 . terminal)
          (terminal . windows-1252)
          (windows-1252 . terminal)
        ))
        (results '())
        (original-encoding (assoc "es" process-coding-system-alist)))
    
    (message "Starting automatic encoding test with query: '%s'" test-query)
    
    (dolist (encoding encodings)
      (message "Testing encoding: %s" encoding)
      (setq process-coding-system-alist `(("es" . ,encoding)))
      
      ;; Ejecutar búsqueda y capturar resultados
      (let ((search-results (org-everything--test-search test-query)))
        (push (cons encoding search-results) results)
        (message "Encoding %s: Found %d results" encoding (length search-results))))
    
    ;; Restaurar configuración original
    (setq process-coding-system-alist (list original-encoding))
    
    ;; Mostrar resultados
    (org-everything--show-encoding-results results)))

(defun org-everything--show-encoding-results (results)
  "Display results of encoding tests in a formatted buffer."
  (let ((buffer (get-buffer-create "*Everything-Encoding-Test-Results*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "=== Everything Encoding Test Results ===\n\n")
      
      (dolist (result results)
        (let ((encoding (car result))
              (search-results (cdr result)))
          (insert (format "Encoding: %s\n" encoding))
                     (insert (format "Results found: %d\n" (length search-results)))
           (insert "Sample results:\n")
           (dolist (line (seq-take search-results 3))
             (insert (format "  %s\n" line)))
           (insert "\n")))
      
      (insert "\n=== Test Complete ===\n")
      (insert "Check the results above to determine the best encoding configuration.\n")
      (insert "Look for configurations that:\n")
      (insert "1. Find the expected number of results\n")
      (insert "2. Display file names with correct accents\n")
      (insert "3. Don't show corrupted characters\n"))
    
    (display-buffer buffer)))

(defcustom org-everything-args
  "es -r"
  "Command line arguments for everything, see `org-everything'.

The default value is \"es -r\", which only works if you place the command line version of Everything (es.exe) in your PATH."
  :type 'string)

;; Performance/options defcustoms (safe defaults: do nothing unless configured)
(defcustom org-everything-max-results nil
  "Maximum number of results to request from es.exe using -n.
When nil, no limit flag is passed and current behavior is preserved."
  :type '(choice (const :tag "Unlimited (default)" nil) integer))

(defcustom org-everything-ignore-case nil
  "When non-nil, add -i to es.exe to perform case-insensitive searches.
Nil preserves existing behavior."
  :type 'boolean)

(defcustom org-everything-default-query-prefix ""
  "Prefix string prepended to user input before sending to es.exe.
Examples: \"ext:pdf \", \"path:src \", \"file:\", \"folder:\", \"size:>1mb \", \"dm:last10days \".
Empty string leaves input unchanged."
  :type 'string)

(defcustom org-everything-extra-args nil
  "Additional raw arguments passed to es.exe after the defaults.
Each element is a single argument. Nil leaves the command unchanged."
  :type '(repeat string))

;; Optional Consult async overrides (nil means: do not override Consult globals)
(defcustom org-everything-consult-min-input nil
  "Minimum number of input characters before starting an asynchronous search in Consult for this command.
When set to a positive integer, this temporarily overrides `consult-async-min-input` while running `org-everything`.

Practical effects:
- With a low value (e.g., 1), searches trigger very early, which can spawn frequent subprocesses and cause flicker on large trees.
- With a moderate value (e.g., 2–3), Emacs waits until the query is more specific before calling `es.exe`, reducing process churn and I/O.
- With nil (default), the global Consult setting is respected and nothing is changed by `org-everything`.

When to adjust:
- Increase to 2–3 if you experience lag or too many rapid updates while typing.
- Keep nil if you already tuned Consult globally and prefer a single place of control."
  :type '(choice (const :tag "Do not override (default)" nil) integer))

(defcustom org-everything-consult-refresh-delay nil
  "Delay (in seconds) before Consult refreshes the candidate list after new input for this command.
When set to a number, this temporarily overrides `consult-async-refresh-delay` while running `org-everything`.

Practical effects:
- Lower values (e.g., 0.05–0.10) refresh results more eagerly, improving perceived responsiveness on small projects.
- Higher values (e.g., 0.15–0.30) batch updates and reduce UI flicker and subprocess pressure on very large codebases.
- Nil (default) leaves the global Consult behavior unchanged.

When to adjust:
- Raise slightly if you notice excessive flicker or CPU usage while typing.
- Lower if your machine is fast and you want more immediate feedback."
  :type '(choice (const :tag "Do not override (default)" nil) number))

(defcustom org-everything-consult-input-throttle nil
  "Minimum time (in seconds) between consecutive asynchronous updates triggered by input for this command.
When set, this temporarily overrides `consult-async-input-throttle` while running `org-everything`.

Practical effects:
- Acts like a rate limiter for updates: a higher value means fewer refreshes per second.
- Useful on networked or very large file sets, where every refresh can be expensive.
- Nil (default) respects the global Consult configuration without local changes.

When to adjust:
- Increase to 0.15–0.30 if your system feels overloaded during rapid typing.
- Keep low or nil if updates are already smooth and inexpensive."
  :type '(choice (const :tag "Do not override (default)" nil) number))

(defcustom org-everything-consult-input-debounce nil
  "Waiting time (in seconds) after the last key press before starting an asynchronous update for this command.
When set, this temporarily overrides `consult-async-input-debounce` while running `org-everything`.

Practical effects:
- Debouncing consolidates bursts of keystrokes into a single refresh; slightly larger values favor stability over immediacy.
- On very fast typists or slow systems, a small debounce (e.g., 0.05–0.15) can noticeably reduce redundant work.
- Nil (default) leaves the global Consult settings untouched.

When to adjust:
- Increase mildly if intermediate updates are rarely helpful while you type complete terms.
- Decrease if you prefer to see results almost instantly after short pauses."
  :type '(choice (const :tag "Do not override (default)" nil) number))

(defcustom org-everything-consult-preview-key nil
  "Preview toggling key for Consult while running `org-everything`.
When non-nil, this temporarily overrides `consult-preview-key` during the command.

What it controls:
- Consult offers an optional live preview of candidates (e.g., showing a file) as you move selection in the minibuffer.
- The preview can be always-on, on-demand (triggered by a specific key), or disabled entirely.

Accepted values:
- nil (default): Do not override; use the global `consult-preview-key`.
- The symbol `any`: Enable preview on any key press that changes the selection.
- A key sequence (string or vector), e.g., "M-.", to show preview only when that key is pressed.
- A list of key sequences: preview is shown when you press any of them.

Performance/UX trade-offs:
- Always-on preview (e.g., `any`) can be helpful for immediate feedback but may open many short-lived previews as you navigate, which can be distracting and wasteful on large projects.
- A dedicated trigger key (e.g., "M-.") is often a sweet spot: you move quickly without preview noise and request a preview on demand.
- Disabling preview (set to an impossible key or leave nil and configure globally) minimizes UI work and maximizes throughput for very large trees.

When to adjust:
- Set to a specific key like "M-." if you want manual, predictable previews while keeping the UI snappy.
- Leave nil if you already manage preview globally in Consult and prefer consistency."
  :type '(choice (const :tag "Do not override (default)" nil)
                 (const :tag "Preview on any key" any)
                 key-sequence
                 (repeat key-sequence)))

;; ===== ONE-SHOT + LOCAL FILTERING VARIANTS =====

(defcustom org-everything-one-shot-args
  "es"
  "Base command for one-shot collection from Everything (es.exe).
This string is parsed into arguments via `consult--build-args` and used to invoke the CLI exactly once to collect an initial candidate set.

Key idea:
- The "one-shot" approach queries Everything only once to collect a finite list of file paths. After that, all interactive filtering happens locally inside Emacs using completion styles (e.g., Orderless), which avoids spawning a new `es.exe` process on every keystroke.

Defaults and safety:
- Default is simply "es" (no regex, no case option). Additional behavior is controlled by the defcustoms below (limit, regex toggle, case toggle, prefix).
- This only affects the alternative command `org-everything-one-shot`. The primary `org-everything` command remains unchanged."
  :type 'string)

(defcustom org-everything-one-shot-limit
  2000
  "Maximum number of results to collect in the one-shot pass (passed as -n).
Higher values provide broader coverage at the cost of a larger in-memory list; lower values return faster and keep the local filtering snappy.

Guidance:
- 1000–5000 is a practical range for modern machines.
- If you routinely search giant trees, start at 2000 and adjust based on responsiveness."
  :type 'integer)

(defcustom org-everything-one-shot-ignore-case
  nil
  "When non-nil, add -i to the one-shot collection so that Everything performs a case-insensitive match for the base query.
Note that this toggle only applies to the initial collection. Subsequent interactive filtering is performed locally in Emacs and follows the active completion style (e.g., Orderless)."
  :type 'boolean)

(defcustom org-everything-one-shot-use-regex
  nil
  "When non-nil, add -r to the one-shot collection to interpret the base query as a regular expression in Everything.
In the one-shot model, regex is typically unnecessary because the bulk of narrowing happens locally. Keep this disabled for maximum speed unless you need a specialized prefilter."
  :type 'boolean)

(defcustom org-everything-one-shot-base-query
  "*"
  "Base query sent to Everything for the one-shot collection, before any interactive narrowing.
Examples:
- "*" (match everything)
- "ext:pdf *" (collect only PDFs)
- "path:src *" (collect only under src)

This base query is also combined with `org-everything-default-query-prefix` if that prefix is non-empty."
  :type 'string)

(defcustom org-everything-one-shot-orderless
  t
  "When non-nil, prefer the Orderless completion style for local filtering, if available.
Behavior:
- If Orderless is installed (`require` succeeds), it will be pushed to the front of `completion-styles` during the `org-everything-one-shot` session, enabling fast, flexible, multi-term filtering without additional subprocesses.
- If Orderless is not installed, nothing breaks; Emacs falls back to your configured completion styles.

Why this helps:
- Local filtering with Orderless excels at quickly slicing a large candidate list using space-separated terms, substrings, and patterns, all without extra calls to `es.exe`."
  :type 'boolean)

(defun org-everything--one-shot-effective-args ()
  "Build argument vector for the one-shot collection based on user options.
Starts from `org-everything-one-shot-args` and appends flags for case, regex, and limit.
`org-everything-extra-args` are also appended for convenience."
  (let* ((args (consult--build-args org-everything-one-shot-args)))
    (when (and org-everything-one-shot-ignore-case
               (not (member "-i" args)))
      (setq args (append args '("-i"))))
    (when (and org-everything-one-shot-use-regex
               (not (member "-r" args)))
      (setq args (append args '("-r"))))
    (when (and (integerp org-everything-one-shot-limit)
               (> org-everything-one-shot-limit 0))
      (setq args (append args (list "-n" (number-to-string org-everything-one-shot-limit)))))
    (when (listp org-everything-extra-args)
      (setq args (append args org-everything-extra-args)))
    args))

(defun org-everything--one-shot-build-query ()
  "Compose the base query for the one-shot pass, applying the optional default prefix."
  (let* ((bq (or org-everything-one-shot-base-query ""))
         (prefix org-everything-default-query-prefix))
    (cond
     ((and (stringp prefix) (not (string-empty-p prefix))
           (stringp bq) (not (string-empty-p bq)))
      (concat prefix bq))
     ((and (stringp prefix) (not (string-empty-p prefix))) prefix)
     ((and (stringp bq) (not (string-empty-p bq))) bq)
     (t "*"))))

;;;###autoload
(defun org-everything-one-shot ()
  "Alternative Everything integration that does a single collection (one-shot) and filters locally.

How it differs from `org-everything`:
- `org-everything` calls Everything on each input change (asynchronously via Consult), excellent for exact fidelity with Everything's matching syntax.
- `org-everything-one-shot` calls Everything once to collect up to `org-everything-one-shot-limit` matches for a base query, then relies on Emacs completion (preferably Orderless) to filter interactively without additional subprocesses.

When to use this:
- You want ultra-responsive narrowing for file names where Everything's advanced operators aren't needed during interactive refinement.
- You prefer to pay the cost of one initial fetch and then enjoy instant, local filtering thereafter.

Notes:
- Encoding and process settings for `es.exe` remain as configured globally (this command does not alter them).
- If Orderless is available and `org-everything-one-shot-orderless` is non-nil, it is preferred locally; otherwise, your existing completion setup is used."
  (interactive)
  (let* ((args (org-everything--one-shot-effective-args))
         (query (org-everything--one-shot-build-query))
         (full (append args (list query)))
         (raw (with-temp-buffer
                (apply #'call-process (car full) nil t nil (cdr full))
                (buffer-string)))
         (candidates (seq-filter (lambda (s) (not (string-empty-p (string-trim s))))
                                 (split-string raw "\n")))
         ;; Prefer Orderless locally if available and requested
         (local-completion-styles (let ((base-styles completion-styles))
                                    (if (and org-everything-one-shot-orderless
                                             (require 'orderless nil t))
                                        (cons 'orderless (remq 'orderless base-styles))
                                      base-styles))))
    (if (null candidates)
        (user-error "Everything (one-shot) returned no candidates for base query: %s" query)
      (let* ((completion-styles local-completion-styles)
             (consult-preview-key (if (not (null org-everything-consult-preview-key))
                                      org-everything-consult-preview-key
                                    consult-preview-key))
             (selection (consult--read candidates
                                       :prompt "Everything (one-shot): "
                                       :category 'file
                                       :require-match t
                                       :sort t)))
        (when selection
          (find-file selection))))))

(defun org-everything--effective-args ()
  "Build the final argument vector for es.exe based on user options.
Starts from `org-everything-args' and appends performance flags when configured."
  (let* ((base (consult--build-args org-everything-args))
         (args base))
    (when (and org-everything-ignore-case
               (not (member "-i" args)))
      (setq args (append args '("-i"))))
    (when (and (integerp org-everything-max-results)
               (> org-everything-max-results 0))
      (setq args (append args (list "-n" (number-to-string org-everything-max-results)))))
    (when (listp org-everything-extra-args)
      (setq args (append args org-everything-extra-args)))
    args))

(defun org--everything-builder (input)
  "Build command line from INPUT."
  ;; Debug: mostrar qué se está enviando
  (message "DEBUG: Búsqueda enviada a Everything: '%s'" input)
  
  (pcase-let ((`(,arg . ,opts) (consult--command-split input)))
    (let* ((final-arg (if (and (stringp org-everything-default-query-prefix)
                               (not (string-empty-p org-everything-default-query-prefix))
                               (not (string-blank-p arg)))
                          (concat org-everything-default-query-prefix arg)
                        arg)))
      (unless (string-blank-p final-arg)
        (cons (append (org-everything--effective-args)
                      (consult--split-escaped final-arg) opts)
              (cdr (consult--default-regexp-compiler input 'basic t)))))))

;;;###autoload
(defun org-everything (&optional initial)
  "Search with `everything' for files matching input regexp given INITIAL input."
  (interactive)
  (let ((consult-async-min-input (if (numberp org-everything-consult-min-input)
                                     org-everything-consult-min-input
                                   consult-async-min-input))
        (consult-async-refresh-delay (if (numberp org-everything-consult-refresh-delay)
                                         org-everything-consult-refresh-delay
                                       consult-async-refresh-delay))
        (consult-async-input-throttle (if (numberp org-everything-consult-input-throttle)
                                          org-everything-consult-input-throttle
                                        consult-async-input-throttle))
        (consult-async-input-debounce (if (numberp org-everything-consult-input-debounce)
                                          org-everything-consult-input-debounce
                                        consult-async-input-debounce))
        (consult-preview-key (if (not (null org-everything-consult-preview-key))
                                 org-everything-consult-preview-key
                               consult-preview-key)))
    (find-file (consult--find "Everything: " #'org--everything-builder initial))))

(provide 'org-everything)

;;; org-everything.el ends here
