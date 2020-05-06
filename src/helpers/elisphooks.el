  (defun agda-index-hook ()
    (when (equal major-mode 'agda2-mode)
      (shell-command (concat "/home/delta/devel/csci5535/project/parser " buffer-file-name)))
    )
  (add-hook 'after-save-hook 'agda-index-hook)

  (defun agda-type-search ()
    (interactive)
    (when (equal major-mode 'agda2-mode)
      ; first rebuild the index, since we have a new query there is no reason to keep an old one
      (shell-command (concat "/home/delta/devel/csci5535/project/parser " buffer-file-name))
      ; then add the query to the index
      (shell-command
       (concat
        "/home/delta/devel/csci5535/project/querybuilder "
        buffer-file-name
        " '"
        (read-string "Enter type signature:")
        "'"
        ))))

