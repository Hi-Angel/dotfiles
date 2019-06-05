(require 'smartparens)


(defun sp-point-not-before-word (id action context)
  "In insert and autoskip actions returns t when next symbol is
not a word constituent."
  (when (or (eq action 'insert) (eq action 'autoskip))
    (looking-at "\\Sw")))

(let ((when '(sp-point-not-before-word))
      (actions  '(list insert wrap autoskip navigate)))
  (sp-pair "{" "}" :when when :actions actions)
  (sp-pair "[" "]" :when when :actions actions)
  (sp-pair "(" ")" :when when :actions actions))

(provide 'sp-sublimelike)
