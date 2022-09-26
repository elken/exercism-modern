;; -*- lexical-binding: t; -*-

(describe "Exercism Tests"

  (describe "Config"
    (it "should get the workspace directory"
      (expect (alist-get 'workspace (exercism-get-config)) :to-equal (expand-file-name "~/Exercism"))))

  (describe "Icons"
    (before-each
      (setq exercism-cache-dir "/tmp/exercism-test"
            exercism--icon-urls '(("i_appear_missing" . "https://httpbin.org/status/403"))
            exercism-missing-icon "https://httpbin.org/uuid")
      (when (file-exists-p exercism-cache-dir)
        (delete-directory exercism-cache-dir t t)))))
