(use-package ellama
  :init
  (setopt ellama-keymap-prefix "C-c e")

  (require 'llm-ollama)
  (setopt ellama-provider (make-llm-ollama :chat-model "mistral-small:24b"))
  )

(provide 'panimacs-llm)
