(use-package ellama
  :init
  (setopt ellama-keymap-prefix "C-c e")

  (require 'llm-ollama)
  (setopt ellama-provider (make-llm-ollama :chat-model "llama3"))
  )

(provide 'panimacs-llm)
