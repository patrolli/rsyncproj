(require 'init-const)

;TODO: monitor sync status from the sync buffer. Maybe detect the line changes or printed time.
(defvar remote-base-path "~/DL_Workspace"
  "parent folder to sync local project")
;; proxychains rsync -auvz  --timeout=5  ~/.emacs.d/site-lisp/rsync-project/ 2:~/DL_Workspace/rsync-project-2;
(defvar rsync-cmd-template "rsync -auvz %s --timeout=5  %s %s:%s/%s;"
  "1: -n 2: local project path 3: remote name 4: remote base path 5: poject name")
;; "\033[31;1;4m$(date)\033[0m\n"
(defvar rsync-add-on-cmd "echo -e \"\\033[31;1;4m$(date)\\033[0m\n\"")
(defvar syp-when-changed-cmd-templeate (if sys/linuxp "when-changed -r -v -1 -s %s -c \"%s\""
					      "wsl ~/.local/bin/when-changed -r -v -1 -s %s -c %s")
    "cmd template for when-changed")
(defvar wsl-home-path "/mnt/c/users/xunsong.li"
  "access windows home path in wsl")

(defun convert-windows-home-to-wsl (fpath)
  "convert C:\\Users\\lixun\\path\\to\\file to /mnt/c/Users/lixun/path/to/file"
  (let* ((str1 (replace-regexp-in-string "[Cc]:\\\\+[Uu]sers\\\\+xunsong\\.li" wsl-home-path fpath))
	 (str2 (replace-regexp-in-string "[Cc]:/+[Uu]sers/+xunsong\\.li" wsl-home-path fpath))
	 (str3 (subst-char-in-string ?\\ ?/ str2)))
    str3))

(defun syp-initialize-project ()
  "Run when-changed and rsync for a project.
It will prompt target remotes according to ssh config file, and
prompt files under project root dir for users to choose to read the
rsync command from it. If there exists a sync buffer to current
project to the chosen remote, we will skip it.
"
  (interactive)
  (let* ((p-path (project-root (project-current t)))
         (remote (completing-read "Remote repo: "
                                  (split-string
                                   (shell-command-to-string
                                    (if sys/linuxp "cat ~/.ssh/config | grep \"^Host \" | awk '{print $2}'"
				      "powershell cat ~/.ssh/config | wsl grep \"^Host \" | wsl awk '{print $2}'")))))
         (p-name (concat (file-name-nondirectory (directory-file-name p-path))
                         "-" remote))
	 (sync-fname (completing-read "sync file: "
				      (directory-files p-path) nil nil ".syncproj")))
      (cond ((syp-is-sync-project p-name) (message "%s is rsyncing!" p-name))
            (t (let ((out-buf (get-buffer-create (format "*my-rsync-%s*" p-name))))
                 (syp-run p-path p-name remote out-buf))))))

(defun syp-dry-run (p-path p-name remote out-buf)
  "Run a test to check which files are rsync."
  (let* ((rsync-cmd (format rsync-cmd-template "-n" p-path remote remote-base-path p-name))
         (cmd (format syp-when-changed-cmd-templeate p-path rsync-cmd)))
    (async-shell-command cmd out-buf nil)))

(defun syp-run (p-path p-name remote out-buf &optional sync-fname)
  " Actual funtion to do when-changed and rsync command. If `sync-fname' is non-nil,
it will parse this file and return its contents as the rsync command to start.
"
  (let* ((sync-f (file-name-concat p-path (or sync-fname ".syncproj")))
         rsync-cmd wc-cmd)
    (if (file-exists-p sync-f)
        (setq rsync-cmd (syp-parse-file sync-f remote))
      (setq rsync-cmd (format rsync-cmd-template "" p-path remote remote-base-path p-name)))
    (if sys/win32p
	(setq p-path (convert-windows-home-to-wsl (file-truename p-path))))
    (setq wc-cmd (format syp-when-changed-cmd-templeate p-path rsync-cmd))
    (async-shell-command wc-cmd out-buf nil)
    (message p-path)))

(defun syp-parse-file (f-path remote)
  "Replace '<remote>' in .syncproj with selected remote name
;TODO: skip lines begin with #"
    (let* ((cmd-tmp (f-read-text f-path))
           (regex "\\\s\\(<remote>\\):"))
      (with-temp-buffer
        (insert cmd-tmp)
        (goto-char (point-min))
        (while (re-search-forward regex nil t)
          (replace-match remote nil nil nil 1))
         (buffer-string))))

(defun syp-list-all-sync-buffers ()
  "get all buffer with name like my-rsync-*"
  (let* (
         (bufs (remove-if-not #'(lambda (x) (string-prefix-p "*my-rsync-" (buffer-name x))) (buffer-list))))
    (or nil bufs)))

(defun syp-list-proj-sync-buffers (proj-name)
  "get sync buffers according to proj-name"
  (if-let (all-bufs (syp-list-sync-projects))
      (cl-remove-if-not #'(lambda (x) (string-match-p proj-name (buffer-name x))) all-bufs)))

(defun syp-goto-sync-buffers ()
  "goto a sync buffer to check the sync state"
  (interactive)
  (if-let ((all-bufs (syp-list-all-sync-buffers))
	   (all-buf-names (mapcar #'buffer-name all-bufs))
	   (chosen-buf (completing-read "choose sync buffer to visit: " all-buf-names)))
      (switch-to-buffer chosen-buf)))

(defun syp-has-exclude-file? (p-path)
  (file-exists-p (concat p-path "/rsync_exclude.txt")))

(defun syp-make-exclude-file (p-path)
  "if exclude.txt does not exist,
  create and jump to it"
  (let* ((fpath (concat p-path "/rsync_exclude.txt")))
    (if (file-exists-p fpath)
        (find-file fpath)
      (with-temp-buffer (write-file fpath nil)))))

(defun syp-is-sync-project (proj-name)
  "check if project (name) being rsync now
  if is, return the rsync buffer, else nil
  TODO: predicate should be improved!"
  (if-let (proj-bufs (syp-list-all-sync-buffers))
      (progn
        (car (cl-remove-if-not #'(lambda (x) (string-match-p proj-name (buffer-name x))) proj-bufs)))))

(defun syp-shutdown--project (proj-name)
  "给定 project 的名称，然后找到其对应的 sync buffer, 然后杀掉这个进程"
  (when-let* ((buf (syp-is-sync-project proj-name)))
    (when-let (process (get-buffer-process buf))
      (kill-process process)
      (sleep-for 0.1)) ;; wait until the process has been killed
    (kill-buffer buf)
    (message "kill rsync project %s" proj-name)))

(defun syp-shutdown--buffer (buf)
  (when-let (process (get-buffer-process buf))
    (kill-process process)
    (sleep-for 0.1)) ;; wait until the process has been killed
  (kill-buffer buf)
  (message "kill rsync buffer %s" (buffer-name buf)))

(defun syp-shutdown-project ()
  (interactive)
  (if-let ((proj-bufs (syp-list-all-sync-buffers))
           (choices (mapcar #'(lambda (x) (apply 'format "%s<%s>" (syp-parse-proc-buf-name (buffer-name x)))) proj-bufs))
           (chosen (completing-read "choose to shutdown: " choices))
           (idx (-elem-index chosen choices))
           (chosen-buf (nth idx proj-bufs)))
      (syp-shutdown--project (car (syp-parse-proc-buf-name (buffer-name chosen-buf))))))

(defun syp-restart-project ()
  "Run this command in a project, and choose a remote to restart sync process."
  (interactive)
  (if-let ((p-path (project-root (project-current t)))
	   (cur-pname (file-name-nondirectory (directory-file-name p-path))) ;; 当前的 proj name
	   (sync-bufs (syp-list-proj-sync-buffers cur-pname))
	   (remotes (mapcar #'(lambda (x) (cdr (syp-parse-proc-buf-name (buffer-name x)))) sync-bufs)) ;; remotes is a nested list: (("remote1"))
	   (chosen-remote (completing-read "choose to shutdown: " remotes))
           (idx (-elem-index chosen-remote (car remotes)))
	   (chosen-buf (nth idx sync-bufs)))
      (progn
	(syp-shutdown--buffer chosen-buf)
	(let ((out-buf (get-buffer-create (format "*my-rsync-%s-%s*" cur-pname chosen-remote))))
	  (syp-run p-path cur-pname chosen-remote out-buf))
	(message (format "Restart sync to %s sucess!" chosen-remote)))))

(defun syp-parse-proc-buf-name (bufname)
  "parse buffer name like *my-rsync-rsync-myProject-Remote* buffer
name into project name (myProject) and remote name (Remote)
"
  (save-match-data
    (if (string-match "\\*my-rsync-\\(.+\\)-\\(.+\\)\\*" bufname)
        (progn (setq proj-name (match-string 1 bufname))
               (setq remote (match-string 2 bufname))
               (list proj-name remote))
      (list nil nil))))

;; tests
;; (syp-shutdown--project "rsync-project")
;; (get-buffer-create "*my-rsync-test*")
;; (kill-process (get-buffer-process (syp-is-sync-project "rsync-project")))
;; (get-buffer-process "*my-rsync-rsync-project*")
;; (kill-buffer "*my-rsync-rsync-project*")
