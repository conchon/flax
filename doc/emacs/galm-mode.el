;;
;; gaml major mode for emacs                                               
;;                                                                          
;; Author: Sylvain Conchon                                                  
;;                                                                          
;; Usage:                                                                   
;;   Copy this file to a location of your load path (e.g. ~/.emacs.d) and add
;;   the following to your .emacs (or .emacs.d/init.el):                     
;;                                                                           
;; ;-----------------                                                        
;; ; mode galm
;; ;-----------------                                            
;; (setq auto-mode-alist                                         
;;       (cons '("\\.galm$" . galm-mode) auto-mode-alist))     
;; (autoload 'galm-mode "galm-mode" "Major mode for galm." t) 
;;                                                               

(defvar galm-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\( ". 1" st)
    (modify-syntax-entry ?\) ". 4" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?_ "w" st)
    st)
  "Syntax table for `galm-mode'.")

(defvar galm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-a" 'galm-find-alternate-file)
    map)
  "Keymap used in Galm mode.")


(defalias 'galm-match-string
  (if (fboundp 'match-string-no-properties)
      'match-string-no-properties
    'match-string))


(defun galm-find-alternate-file ()
  "Switch Galm/Galm."
  (interactive)
  (let ((name (buffer-file-name)))
    (when (string-match "\\`\\(.*\\)\\.galm\\'"name)
      (find-file (concat (galm-match-string 1 name)
                         (if (match-beginning 2) ".galm" ".flax")))))
  )


(defvar galm-font-lock-keywords
  '(
    ("(\\*\\([^*]\\|\\*[^)]\\)*\\*)" . font-lock-comment-face)
    ; transitions need not have a return type
    ("\\(rule\\)\\s-+\\(\\sw+\\)" 
       (1 font-lock-keyword-face) (2 font-lock-function-name-face))
    ("\\b\\(loc_in\\)\\b" (1 font-lock-variable-name-face))
    ("\\b\\(loc_out\\)\\b" (1 font-lock-variable-name-face))
    ("\\b\\(guards\\)\\b" (1 font-lock-type-face))
    ("\\b\\(actions\\)\\b" (1 font-lock-type-face))
    ("\\b\\(State\\)\\b" (1 font-lock-type-face))
    ("\\b\\(Apply\\)\\b" (1 font-lock-type-face))
    ("\\b\\(Fabric\\)\\b" (1 font-lock-variable-name-face))
    ("\\*\\*\\*\\(.*\\)\\*\\*\\*" (1 font-lock-variable-name-face))
    ("\\[\\(.*\\)*\\]" . font-lock-function-name-face)
    "Keyword highlighting specification for `galm-mode'."))

(require 'compile)
(define-derived-mode galm-mode fundamental-mode "galm"
  "A major mode for editing galm files."
  :syntax-table galm-mode-syntax-table
  (set (make-local-variable 'comment-start) "(*")  
  (set (make-local-variable 'comment-end) "*)")
  (when (buffer-file-name)
    (set (make-local-variable 'compile-command)
         (format "galm %s" (file-name-nondirectory buffer-file-name))))
  (set (make-local-variable 'font-lock-defaults)'(galm-font-lock-keywords))
  (interactive)
  (setq major-mode 'galm-mode)
  (setq mode-name "Galm")
  (use-local-map galm-mode-map)
)

(provide 'galm-mode)
