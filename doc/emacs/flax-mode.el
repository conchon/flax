;;
;; iflow major mode for emacs                                               
;;                                                                          
;; Author: Sylvain Conchon                                                  
;;                                                                          
;; Usage:                                                                   
;;   Copy this file to a location of your load path (e.g. ~/.emacs.d) and add
;;   the following to your .emacs (or .emacs.d/init.el):                     
;;                                                                           
;; ;-----------------                                                        
;; ; mode iflow                                                           
;; ;-----------------                                            
;; (setq auto-mode-alist                                         
;;       (cons '("\\.iflow$" . iflow-mode) auto-mode-alist))     
;; (autoload 'iflow-mode "iflow-mode" "Major mode for iflow." t) 
;;                                                               

(defvar flax-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\( ". 1" st)
    (modify-syntax-entry ?\) ". 4" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?_ "w" st)
    st)
  "Syntax table for `flax-mode'.")

(defvar flax-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-a" 'flax-find-alternate-file-galm)
    (define-key map "\C-c\C-b" 'flax-find-alternate-file-cub)
    (define-key map "\C-c\C-p" 'flax-find-alternate-file-pml)
    map)
  "Keymap used in Flax mode.")


(defalias 'flax-match-string
  (if (fboundp 'match-string-no-properties)
      'match-string-no-properties
    'match-string))


(defun flax-find-alternate-file-galm ()
  "Switch Flax/Galm."
  (interactive)
  (let ((name (buffer-file-name)))
    (when (string-match "\\`\\(.*\\)\\.flax\\'"name)
      (find-file (concat (flax-match-string 1 name)
                         (if (match-beginning 2) ".flax" ".galm")))))
  )


(defun flax-find-alternate-file-cub ()
  "Switch Flax/Cubicle."
  (interactive)
  (let ((name (buffer-file-name)))
    (when (string-match "\\`\\(.*\\)\\.flax\\'"name)
      (find-file (concat (flax-match-string 1 name)
                         (if (match-beginning 2) ".flax" ".cub")))))
  )

(defun flax-find-alternate-file-pml ()
  "Switch Flax/Promela."
  (interactive)
  (let ((name (buffer-file-name)))
    (when (string-match "\\`\\(.*\\)\\.flax\\'"name)
      (find-file (concat (flax-match-string 1 name)
                         (if (match-beginning 2) ".flax" ".pml")))))
  )


(defvar flax-font-lock-keywords
  '(
    ("(\\*\\([^*]\\|\\*[^)]\\)*\\*)" . font-lock-comment-face)
    ; transitions need not have a return type
    ("\\(agent\\)\\s-+\\(\\sw+\\)" 
       (1 font-lock-keyword-face) (2 font-lock-function-name-face))
    ("\\b\\(repository\\)\\b" (1 font-lock-variable-name-face))
    ("\\b\\(local\\)\\b" (1 font-lock-type-face))
    ("\\b\\(global\\)\\b" (1 font-lock-type-face))

    ("\\(diagram\\)\\s-+\\(\\sw+\\)" 
       (1 font-lock-keyword-face) (2 font-lock-function-name-face))

    ("\\b\\(node\\)\\b" (1 font-lock-variable-name-face))
    ("\\b\\(arc\\)\\b" (1 font-lock-type-face))

    ("\\b\\(GENERIC\\)\\b" (1 font-lock-function-name-face))
    ("\\b\\(IOSF_SB\\)\\b" (1 font-lock-function-name-face))
    ("\\b\\(WIRE\\)\\b" (1 font-lock-function-name-face))

    ("\\b\\(P\\)\\b" (1 font-lock-function-name-face))
    ("\\b\\(NP\\)\\b" (1 font-lock-function-name-face))
    ("\\b\\(C\\)\\b" (1 font-lock-function-name-face))

    ("\\b\\(START\\)\\b" (1 font-lock-function-name-face))
    ("\\b\\(TASK\\)\\b" (1 font-lock-function-name-face))
    ("\\b\\(PSYNC\\)\\b" (1 font-lock-function-name-face))
    ("\\b\\(NPSYNC\\)\\b" (1 font-lock-function-name-face))
    ("\\b\\(BRANCH\\)\\b" (1 font-lock-function-name-face))
    ("\\b\\(SEQUENCE\\)\\b" (1 font-lock-function-name-face))
    ("\\b\\(MESSAGE\\)\\b" (1 font-lock-function-name-face))

    "Keyword highlighting specification for `flax-mode'."))

(require 'compile)
(define-derived-mode flax-mode fundamental-mode "flax"
  "A major mode for editing flax files."
  :syntax-table flax-mode-syntax-table
  (set (make-local-variable 'comment-start) "(*")  
  (set (make-local-variable 'comment-end) "*)")
  (when (buffer-file-name)
    (set (make-local-variable 'compile-command)
         (format "flax %s" (file-name-nondirectory buffer-file-name))))
  (set (make-local-variable 'font-lock-defaults)'(flax-font-lock-keywords))
  (interactive)
  (setq major-mode 'flax-mode)
  (setq mode-name "Flax")
  (use-local-map flax-mode-map)
)

(provide 'flax-mode)
