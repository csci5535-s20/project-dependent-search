  (defun agda-index-hook ()
    (when (equal major-mode 'agda2-mode)
      (shell-command (concat "/path/to/parser " buffer-file-name)))
    )
  ; (add-hook 'after-save-hook 'agda-index-hook) ; optional on-save hook to always build an index, not necessary

  (defun agda-type-search ()
    (interactive)
    (when (equal major-mode 'agda2-mode)
      ; first rebuild the index, since we have a new query there is no reason to keep an old one
      (shell-command (concat "/path/to/parser " buffer-file-name))
      ; then add the query to the index
      (shell-command
       (concat
        "/path/to/querybuilder "
        buffer-file-name
        " '"
        (read-string "Enter type signature:")
        "'"
        ))))

