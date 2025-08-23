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
(require 'subr-x)
(require 'seq)

;; Configuración de codificación para Everything (por defecto cp850 -> utf-8)
(add-to-list 'process-coding-system-alist '("es" . (cp850 . utf-8)))

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
        (assoc-delete-all "es" process-coding-system-alist))
  
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

The default value is \"es -r\", which works if the Everything command line (es.exe) is in your PATH."
  :type 'string)

(defun org--everything-builder (input)
  "Build command line from INPUT."
  ;; Debug: mostrar qué se está enviando
  (message "DEBUG: Búsqueda enviada a Everything: '%s'" input)
  
  (pcase-let ((`(,arg . ,opts) (consult--command-split input)))
    (unless (string-blank-p arg)
      (cons (append (consult--build-args org-everything-args)
                    (consult--split-escaped arg) opts)
            (cdr (consult--default-regexp-compiler input 'basic t))))))

;;;###autoload
(defun org-everything (&optional initial)
  "Search with `everything' for files matching input regexp given INITIAL input."
  (interactive)
  (find-file (consult--find "Everything: " #'org--everything-builder initial)))

(defun org-everything--string-count-regexp (string regexp)
  "Return number of non-overlapping matches of REGEXP in STRING."
  (let ((pos 0)
        (count 0))
    (while (and (< pos (length string))
                (string-match regexp string pos))
      (setq count (1+ count))
      (setq pos (match-end 0)))
    count))

(defun org-everything--string-count-chars (string chars)
  "Count how many characters from CHARS occur in STRING. CHARS is a string."
  (let ((table (make-hash-table :test 'eq))
        (i 0)
        (total 0))
    (while (< i (length chars))
      (puthash (aref chars i) t table)
      (setq i (1+ i)))
    (setq i 0)
    (while (< i (length string))
      (when (gethash (aref string i) table)
        (setq total (1+ total)))
      (setq i (1+ i)))
    total))

(defun org-everything--args-has-codepage-65001-p (args)
  "Return non-nil if ARGS list contains -codepage 65001."
  (let ((has nil)
        (i 0))
    (while (< i (length args))
      (when (and (string-equal (nth i args) "-codepage")
                 (string-equal (or (nth (1+ i) args) "") "65001"))
        (setq has t)
        (setq i (length args)))
      (setq i (1+ i)))
    has))

(defun org-everything--args-remove-codepage (args)
  "Remove any -codepage <value> from ARGS list."
  (let ((out '())
        (i 0))
    (while (< i (length args))
      (let ((a (nth i args)))
        (if (and (string-equal a "-codepage")
                 (< (1+ i) (length args)))
            (setq i (+ i 2))
          (progn
            (setq out (append out (list a)))
            (setq i (1+ i))))))
    out))

(defun org-everything--split-org-args ()
  "Split `org-everything-args' into (PROGRAM . ARGS)."
  (let* ((parts (split-string org-everything-args "[ \t]+" t))
         (program (car parts))
         (args (cdr parts)))
    (cons program args)))

(defun org-everything--build-command (with-codepage &optional query)
  "Build command list for Everything CLI. WITH-CODEPAGE forces -codepage 65001 if non-nil.
If WITH-CODEPAGE is nil, any -codepage is removed. Append QUERY if non-nil."
  (pcase-let* ((`(,program . ,args) (org-everything--split-org-args))
               (args* (cond
                       (with-codepage
                        (if (org-everything--args-has-codepage-65001-p args)
                            args
                          (append args (list "-codepage" "65001"))))
                       (t
                        (org-everything--args-remove-codepage args))))
               (final (if (and query (not (string-empty-p query)))
                          (append args* (list query))
                        args*)))
    (cons program final)))

(defun org-everything--args-has-flag (flag)
  "Return non-nil if `org-everything-args' contains FLAG."
  (let* ((parts (split-string org-everything-args "[ \t]+" t)))
    (member flag parts)))

(defun org-everything--test-queries ()
  "Return a robust list of queries for auto-fix tests depending on -r flag."
  (if (org-everything--args-has-flag "-r")
      '(".+" ".*ñ.*" ".*ó.*" ".*")
    '("*" "*ñ*" "*ó*" "*")))

(defun org-everything--run-cli (coding-in coding-out with-codepage query)
  "Run Everything CLI with CODING-IN/CODING-OUT for process, controlling codepage flag.
Return output string."
  (pcase-let* ((`(,program . ,args0) (org-everything--build-command with-codepage query))
               (args (if (member "-n" args0) args0 (append args0 '("-n" "50")))))
    (let ((process-coding-system-alist `(("es" . (,coding-in . ,coding-out))))
          (default-process-coding-system `(,coding-in . ,coding-out)))
      (with-temp-buffer
        (apply #'call-process program nil t nil args)
        (buffer-string)))))

(defun org-everything--score-output (output)
  "Return a plist with :score and metrics for OUTPUT heuristics."
  (let* ((good-chars "áéíóúüñÁÉÍÓÚÜÑ")
         (good (org-everything--string-count-chars output good-chars))
         (bad (+ (org-everything--string-count-regexp output "\\\\\[0-9]+")
                 (org-everything--string-count-regexp output "¢")
                 (org-everything--string-count-regexp output "à")
                 (org-everything--string-count-regexp output "Ã")))
         (lines (length (split-string output "\n" t)))
         (score (+ (* 3 good) (* 1 (min lines 10)) (* -5 bad)
                   (if (= lines 0) -1000 0))))
    (list :good good :bad bad :lines lines :score score)))

(defconst org-everything-auto-strategies
  '((:name "utf8+codepage" :coding-in utf-8 :coding-out utf-8 :with-codepage t)
    (:name "utf8+codepage+undecided-in" :coding-in undecided :coding-out utf-8 :with-codepage t)
    (:name "cp1252->utf8" :coding-in cp1252 :coding-out utf-8 :with-codepage nil)
    (:name "win-1252->utf8" :coding-in windows-1252 :coding-out utf-8 :with-codepage nil)
    (:name "cp850->utf8" :coding-in cp850 :coding-out utf-8 :with-codepage nil)
    (:name "iso-8859-1->utf8" :coding-in iso-8859-1 :coding-out utf-8 :with-codepage nil)
    (:name "utf8-no-codepage" :coding-in utf-8 :coding-out utf-8 :with-codepage nil))
  "List of encoding strategies to test automatically.")

(defun org-everything--ensure-accent-probes (queries)
  "Ensure QUERIES includes probes with ñ/ó to test encoding."
  (let ((need-accents (not (seq-some (lambda (q) (string-match-p "[ñÑóÓ]" q)) queries))))
    (if need-accents
        (append queries (if (org-everything--args-has-flag "-r") '(".*ñ.*" ".*ó.*") '("*ñ*" "*ó*")))
      queries)))

(defun org-everything-auto-fix-encodings (&optional query)
  "Automatically try encoding strategies and apply the best one. Optional QUERY to test."
  (interactive (list (read-string "Query for test (default auto): " nil nil "")))
  (let* ((queries0 (if (and query (not (string-empty-p query))) (list query) (org-everything--test-queries)))
         (queries1 (org-everything--ensure-accent-probes queries0))
         (queries (delete-dups (append queries1 (org-everything--test-queries))))
         (best nil)
         (best-metrics nil)
         (report (get-buffer-create "*Everything-Auto-Fix*")))
    (with-current-buffer report
      (erase-buffer)
      (insert "=== Everything Auto Encoding Fix ===\n\n"))
    (dolist (strategy org-everything-auto-strategies)
      (let* ((coding-in (plist-get strategy :coding-in))
             (coding-out (plist-get strategy :coding-out))
             (with-codepage (plist-get strategy :with-codepage))
             (name (plist-get strategy :name))
             (agg-score 0)
             (agg-good 0)
             (agg-bad 0)
             (agg-lines 0))
        (dolist (q queries)
          (let* ((out (condition-case _
                           (org-everything--run-cli coding-in coding-out with-codepage q)
                         (error "")))
                 (m (org-everything--score-output out)))
            (setq agg-score (+ agg-score (plist-get m :score)))
            (setq agg-good (+ agg-good (plist-get m :good)))
            (setq agg-bad (+ agg-bad (plist-get m :bad)))
            (setq agg-lines (+ agg-lines (plist-get m :lines)))))
        (with-current-buffer report
          (insert (format "- %s: score=%d good=%d bad=%d lines=%d\n"
                          name agg-score agg-good agg-bad agg-lines)))
        (let ((metrics (list :score agg-score :good agg-good :bad agg-bad :lines agg-lines)))
          (when (or (null best)
                    (> (plist-get metrics :lines) (plist-get best-metrics :lines))
                    (and (= (plist-get metrics :lines) (plist-get best-metrics :lines))
                         (< (plist-get metrics :bad) (plist-get best-metrics :bad)))
                    (and (= (plist-get metrics :lines) (plist-get best-metrics :lines))
                         (= (plist-get metrics :bad) (plist-get best-metrics :bad))
                         (> (plist-get metrics :good) (plist-get best-metrics :good)))))
            (setq best strategy)
            (setq best-metrics metrics)))))
    (with-current-buffer report
      (insert "\n"))
    (if (or (null best) (= (plist-get best-metrics :lines) 0))
        (progn
          (with-current-buffer report
            (insert "No viable strategy found (no lines). Keeping current settings.\n"))
          (display-buffer report)
          (message "Everything auto-fix: no strategy applied"))
      (let* ((strategy best)
             (coding-in (plist-get strategy :coding-in))
             (coding-out (plist-get strategy :coding-out))
             (with-codepage (plist-get strategy :with-codepage))
             (args-parts (split-string org-everything-args "[ \t]+" t))
             (program (car args-parts))
             (args (cdr args-parts))
             (new-args (if with-codepage
                           (if (org-everything--args-has-codepage-65001-p args)
                               args
                             (append args (list "-codepage" "65001")))
                         (org-everything--args-remove-codepage args)))
             (new-org-args (mapconcat #'identity (cons program new-args) " ")))
        (setq org-everything-args new-org-args)
        (setq process-coding-system-alist (assoc-delete-all "es" process-coding-system-alist))
        (add-to-list 'process-coding-system-alist (cons "es" (cons coding-in coding-out)))
        (with-current-buffer report
          (insert (format "Applied strategy: %s\n" (plist-get strategy :name)))
          (insert (format "org-everything-args: %s\n" org-everything-args))
          (insert (format "process-coding-system-alist[es]: (%s . %s)\n" coding-in coding-out)))
        (display-buffer report)
        (message "Everything auto-fix applied: %s" (plist-get strategy :name))))))

(defun org-everything-auto-run (&optional initial)
  "Run auto-fix for encodings, then start `org-everything' with INITIAL input."
  (interactive)
  (org-everything-auto-fix-encodings "*")
  (call-interactively 'org-everything))

(provide 'org-everything)

;;; org-everything.el ends here
