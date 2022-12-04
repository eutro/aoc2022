((nil . ((indent-tabs-mode . nil)
         (eval . (when (and
                        (buffer-file-name)
                        (not (file-directory-p (buffer-file-name)))
                        (string-match-p "^.+\\.pl$" (buffer-file-name)))
                   (unless (derived-mode-p 'prolog-mode)
                     (prolog-mode)))))))
