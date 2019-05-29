(require 'smartparens)


(defun sp-point-before-closing-brace (id action context)
  "Return t if point is followed by a closing brace. Currently
it's either of «)» «}». This predicate is only tested on
\"insert\" action."
  (when (or (eq action 'insert) (eq action 'autoskip))
    (looking-at "\\s)\\|\}")))

(defun sp-point-before-whitespace (id action context)
  "Return t if point is followed by a whitespace or newline.
This predicate is only tested on \"insert\" action."
  (when (or (eq action 'insert) (eq action 'autoskip))
    (looking-at "\\s-\\|$")))

(let ((when '(sp-point-before-whitespace sp-point-before-closing-brace))
      (actions  '(list insert wrap autoskip navigate)))
  (sp-pair "{" "}" :when when :actions actions)
  (sp-pair "[" "]" :when when :actions actions)
  (sp-pair "(" ")" :when when :actions actions))

(provide 'sp-sublimelike)
