;; .comfig.el

(setq compilation-auto-jump-to-first-error t)

;; Compile command using Projectile root
(defun call/MakeFile-test ()
  "Run 'make -j3 run' from the Projectile project root."
  (interactive)
  (if-let ((project-root (projectile-project-root)))
      (let ((default-directory project-root))
        (compile "make -j3 run"))
    (message "Not in a Projectile project.")))

;; Bind to C-c m t
(global-set-key (kbd "C-c m t") #'call/MakeFile-test)
