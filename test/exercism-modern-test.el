;; -*- lexical-binding: t; -*-

(describe "Exercism Tests"
  (describe "Config"
    (it "should get the workspace directory"
      (expect (downcase (alist-get 'workspace (exercism-modern-get-config))) :to-equal (expand-file-name "~/exercism"))))

  (describe "Icons"
    (before-each
      (setq exercism-modern-cache-dir "/tmp/exercism-test"
            exercism-modern--icon-urls '(("i_appear_missing" . "https://httpbin.org/status/403"))
            exercism-modern-missing-icon "https://httpbin.org/uuid")
      (when (file-exists-p exercism-modern-cache-dir)
        (delete-directory exercism-modern-cache-dir t t)))))
