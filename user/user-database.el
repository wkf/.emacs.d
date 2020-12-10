;;; user.database --- project config
;;; Commentary:
;;; Code:


;; (assoc 'dtour-staging user/databases)

;; (print user/databases)
;; (print (equal 'dtour-staging 'dtour-staging))
;; (print (alist-get 'dtour-staging user/databases))

;; (print (alist-get 'dtour-staging user/databases))

;; (makunbound 'user/databases)
;; user/databases
;; sql-connection-alist
;; (car (alist-get 'sql-product (alist-get 'dtour-staging sql-connection-alist)))

(defun user/-sql-connect (new)
  "Connect to sql, creating a new buffer if NEW is t."
  (if (get-buffer "*SQL*")
      (switch-to-buffer-other-window "*SQL*")
    (ivy-read "Connect: " sql-connection-alist
              :action (lambda (db)
                        (setq sql-product
                              (->> sql-connection-alist
                                   (alist-get (car db))
                                   (alist-get 'sql-product)
                                   (cadar)))
                        (if new
                            (sql-connect (car db) (car db))
                          (sql-connect (car db)))))))

(defun user/sql-connect ()
  "Connect to a sql database."
  (interactive)
  (user/-sql-connect nil))

(defun user/sql-new-connect ()
  "Connect to a sql database."
  (interactive)
  (user/-sql-connect t))

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

(provide 'user-database)
;;; user-database.el ends here
