;;; panimacs-workspaces.el --- panimacs :: workspaces — -*- lexical-binding: t -*-


(defvar panimacs/initial-scratch-message
  "* The **scratch** buffer

** Quick *code*

#+begin_src elisp :results output
(message \"Welcome to the *scratch* buffer!\")
#+end_src

#+begin_src cpp :results output
#include <iostream>

int main() {
    std::cout << \"Welcome to the *scratch* buffer!\\n\";
}
#+end_src

#+begin_src python :results output
print(\"Welcome to the *scratch* buffer!\")
#+end_src

** Quick *notes*

")

(use-package persp-mode
  :init (persp-mode +1)
  :config
  (setq persp-init-new-frame-behaviour-override (lambda (&rest args)))

  (setq initial-major-mode 'org-mode)
  (setq initial-scratch-message panimacs/initial-scratch-message)
  )

(provide 'panimacs-workspaces)
