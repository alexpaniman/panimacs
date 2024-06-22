(use-package ellama
  :init
  ;; setup key bindings
  (setopt ellama-keymap-prefix "C-c e")

  (require 'llm-ollama)
  (setopt ellama-provider
          (make-llm-ollama :chat-model "llama3:70b")
	  ;; (make-llm-ollama
	  ;;  ;; this model should be pulled to use it
	  ;;  ;; value should be the same as you print in terminal during pull
	  ;;  :chat-model "llama3-gradient"
	  ;;  :embedding-model "nomic-embed-text"
	  ;;  :default-chat-non-standard-params '(("num_ctx" . 90000)))
          )
  )

(provide 'panimacs-llm)
