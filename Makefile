boot:
	@emacsclient --socket-name=termserver --suppress-output \
	  --eval \
		"(with-current-buffer (car (find-file-noselect \"./*.org\" nil nil t)) \
			(goto-char (point-min)) \
			(re-search-forward \"^#[+]name:preprocess.el$$\") \
			(org-babel-tangle (quote (4))) \
			(save-buffer))" \
	  --eval \
		"(let ((rsrcdir \"resources\") \
		       (subdirs (list \"tools\" \"images\" \"source\"))) \
		   (mkdir rsrcdir t) \
		   (dolist (subdir subdirs) (mkdir (concat rsrcdir \"/\" subdir) t)))"
	@./resources/tools/preprocess.el
git:
	git add . && git commit -m "After running boot-template Makefile" && git push origin master
all: boot git
