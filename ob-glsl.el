(require 'ob)
(require 'ob-glsl-module)

(defvar org-babel-default-header-args:glsl
  '((:results . "file") (:exports . "results"))
  "Default arguments to use when evaluating a glsl source block.")

(defun org-babel-expand-body:glsl (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (concat
   "#version 330 core\n"
   "out vec4 fragColor;\n"
   "uniform vec2 iResolution;\n"
   body))

(defun org-babel-execute:glsl (body params)
  "Execute a block of glsl code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let ((out-file (org-babel-process-file-name
                   (cdr (or (assq :file params)
			                (error "You need to specify a :file parameter")))
                   t))
        (glsl-code (org-babel-expand-body:glsl body params))
        (render-width (cdr (assq :width params)))
        (render-height (cdr (assq :height params))))
    (if (and (not render-width) render-height)
        (setq render-width (* 4 (/ render-height 3))))
    (if (and (not render-height) render-width)
        (setq render-height (* 3 (/ render-width 4))))
    (if (and (not render-width) (not render-height))
        (setq render-width 400 render-height 300))
    (ob-glsl-run glsl-code render-width render-height out-file)
    nil))

(defun org-babel-prep-session:shady (_session _params)
  "Return an error because glsl does not support sessions."
  (error "glsl does not support sessions"))

(provide 'ob-glsl)
