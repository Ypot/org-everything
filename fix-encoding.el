;;; fix-encoding.el --- Script rápido para corregir acentos en org-everything

(defun fix-everything-encoding ()
  "Diagnóstico rápido y corrección de codificación para Everything."
  (interactive)
  (let ((test-query "*.txt") ; Busca archivos .txt que probablemente existan
        (encodings-to-test '(
          ;; Las más probables para Windows en español
          cp850    ; DOS España
          cp1252   ; Windows España
          iso-8859-1  ; ISO Latin-1
          iso-8859-15 ; ISO Latin-9 (con €)
          utf-8    ; UTF-8
          windows-1252 ; Alias de cp1252
        ))
        (buffer-name "*Fix Everything Encoding*")
        (best-encoding nil))
    
    (with-output-to-temp-buffer buffer-name
      (princ "=== DIAGNÓSTICO RÁPIDO DE CODIFICACIÓN ===\n\n")
      (princ "Probando codificaciones más comunes...\n\n")
      
      ;; Probar cada codificación
      (dolist (encoding encodings-to-test)
        (princ (format "Probando %s: " encoding))
        (condition-case err
            (let* ((process-coding-system-alist `(("es" . (,encoding . utf-8))))
                   (result (with-temp-buffer
                            (call-process "es" nil t nil "-r" "-n" "5" test-query)
                            (buffer-string))))
              (if (string-empty-p (string-trim result))
                  (princ "Sin resultados\n")
                (let ((first-line (car (split-string result "\n"))))
                  (princ (format "Resultado: %s\n" 
                               (if (> (length first-line) 60)
                                   (concat (substring first-line 0 57) "...")
                                 first-line)))
                  ;; Verificar si hay caracteres problemáticos
                  (unless (or (string-match "\\\\[0-9]+" first-line)
                             (string-match "[¢àáéíóúñÑ]" first-line))
                    (unless best-encoding
                      (setq best-encoding encoding))))))
          (error (princ (format "ERROR: %s\n" (error-message-string err))))))
      
      (princ "\n=== RESULTADO ===\n")
      (if best-encoding
          (progn
            (princ (format "✓ Codificación recomendada: %s\n" best-encoding))
            (princ "\n¿Aplicar esta codificación? Ejecuta:\n")
            (princ (format "(org-everything-apply-encoding-fix '%s)\n" best-encoding)))
        (progn
          (princ "✗ No se encontró una codificación óptima automáticamente\n")
          (princ "\nEjecuta el diagnóstico manual:\n")
          (princ "M-x org-everything-raw-bytes-test\n"))))
    
    ;; Aplicar automáticamente si encontramos una buena codificación
    (when best-encoding
      (when (y-or-n-p (format "¿Aplicar automáticamente la codificación %s? " best-encoding))
        (org-everything-apply-encoding-fix best-encoding)
        (message "✓ Codificación %s aplicada. Prueba M-x org-everything ahora." best-encoding)))))

(defun test-current-encoding ()
  "Prueba rápida de la codificación actual."
  (interactive)
  (let ((result (with-temp-buffer
                 (call-process "es" nil t nil "-r" "-n" "3" "*")
                 (buffer-string))))
    (if (string-empty-p (string-trim result))
        (message "No hay resultados - verifica que Everything esté funcionando")
      (with-output-to-temp-buffer "*Current Encoding Test*"
        (princ "=== PRUEBA DE CODIFICACIÓN ACTUAL ===\n\n")
        (princ "Primeros resultados:\n")
        (princ result)
        (princ "\n¿Se ven bien los acentos? Si no, ejecuta: M-x fix-everything-encoding")))))

;; Función de un solo paso para usuarios
(defun everything-fix-accents ()
  "Solución en un paso para corregir acentos en Everything."
  (interactive)
  (message "Diagnosticando problema de acentos...")
  (fix-everything-encoding))

(provide 'fix-encoding)