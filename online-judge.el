;;; online-judge.el --- Interface for online-judge-tool.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: tools

;; Version: 1.1.14
;; Package-Requires: ((f "0.20.0") (dash "2.14"))
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'f)

(defgroup online-judge ()
  "Group for online-judge."
  :group 'tools
  :prefix "online-judge-")

(defvar-local online-judge--host nil)

(defvar-local online-judge--contest nil)

(defvar-local online-judge--problem nil)

(defvar-local online-judge--url nil)

(defvar-local online-judge--test-downloading nil
  "If on downloading test, it have process object.
If not, it have nil.")

(defvar-local online-judge--test-downloaded nil
  "Testcases are downloaded or not in the buffer.")

(defvar-local online-judge--set nil
  "All information of the problem is set or not.")

(defvar-local online-judge--error-range 0)

(defvar-local online-judge--error-range-before nil)

(defvar-local online-judge--sample-format nil)

(defvar-local online-judge--this-url nil)



(defvar online-judge--buffer-name "*online-judge*")

(defvar online-judge--host-alist
  `((atcoder .
             (:host "[Aa]t[Cc]oder"
              :home "https://atcoder.jp"
              :contest "[-_a-zA-Z0-9]+"
              :contest-exact
              ,(concat
                "\\(\\([Aa][GgBbRr][Cc]\\|[Cc]hokudai\\|CHOKUDAI\\)[0-9]+\\)\\|"
                "\\(past[0-9]+\\(-open\\)?\\)")
              :problem "[A-Za-z]"
              :url (online-judge--atcoder-url)))
    ))

(defvar online-judge--login-alist
  (mapcar (lambda (arg) (list (car arg))) online-judge--host-alist))

(defvar online-judge-test-mode-map (make-sparse-keymap)
  "Keymap of `online-judge-test-mode.'")

(defvar online-judge-oj-test-mode-key-alist
  '(("TAB" . outline-toggle-children)))

(defconst online-judge--base-sample-format "sample-%i.%e")



(defcustom online-judge-executable
  (if (eq system-type 'windows-nt) "oj.exe" "oj")
  "Executable command for \"online-judge-tool\".
You can install this from https://github.com/kmyk/online-judge-tools. "
  :type 'string
  :group 'online-judge
  :link '(url-link https://github.com/kmyk/online-judge-tools))

(defcustom online-judge-1-directory-p t
  "Add all sample cases to 1 directory, if non-nil.
If you want to play 1 contest in 1 directory, you should make this non-nil.
When this variable is non-nil, samples are prefixed problem name (such as A, B,
in atcoder, for example)."
  :type 'bool
  :group 'online-judge)

(defcustom online-judge-command-name
  (if (eq system-type 'windows-nt) "a.exe" "a.out")
  "If non-nil, this should be executable name for tested,
or sexp which return such a executable name.
When nil, it is regarded as (`buffer-file-name') (or (`buffer-file-name').exe
in windows)."
  :group 'online-judge
  :type [string nil])

(defcustom online-judge-default-host 'atcoder
  "Default host. online-judge-mode uses this host if only put enter on choice.
Choice from car of each element of `online-judge--host-alist'."
  :group 'online-judge
  :type `(choice ,@(cl-mapcar
                    (lambda (arg)
                      `(const ,(car arg)))
                    online-judge--host-alist)))

(defcustom online-judge-ask-when-unconfirmed t
  "If t, You are asked what host you use, when cannot confirm host from dir."
  :group 'online-judge
  :type 'bool)

(defcustom online-judge-separator-1 "/"
  "Separator between host and contest on mode-line.
Only use when online-judge-display-mode-line is t."
  :group 'online-judge
  :type 'string)

(defcustom online-judge-separator-2 "/"
  "Separator between contest and problem on mode-line.
Only use when online-judge-display-mode-line is t or symbol contest."
  :group 'online-judge
  :type 'string)

(defcustom online-judge-display-mode-line t
  "This determine how long infomation is displayed on mode-line.
You can set this variable to:
	t: Display host, contest, and problem.
	contest: Display contest and problem.
	problem: Display problem.
	nil: Display none."
  :group 'online-judge
  :type '(choose (const t)
                 (const contest)
                 (const problem)
                 (const nil)))

(defcustom online-judge-directories '()
  "`online-judge-mode' become automatically on if the file is children of element of this."
  :group 'online-judge
  :type '(repeat string))

(defcustom online-judge-error-range-default 1e-6
  "Default error range on test.
You can toggle or change error range interactively with
`online-judge-toggle-error-range' and `online-judge-update-error-range'."
  :group 'online-judge
  :type 'number)

(defcustom online-judge-mode-line-list '(:eval (online-judge--mode-line))
  "Mode line list which is added to `mode-line-format'."
  :group 'online-judge
  :type 'list
  :risky t
  :set (lambda (variable value)
         (if (not (bound-and-true-p online-judge-mode))
             (set variable value)
           (delete value mode-line-format)
           (setq-default mode-line-format
                         (append
                          mode-line-format
                          (list (set variable value)))))))



(defun online-judge--test-outline-mode ()
  ""
  (setq outline-regexp "# [-0-9: ]*")
  (outline-minor-mode))

(defun online-judge--set-keys ()
  ""
  (mapcar (lambda (arg)
            (define-key online-judge-test-mode-map (kbd (car arg)) (cdr arg)))
          online-judge-oj-test-mode-key-alist)
  (use-local-map online-judge-test-mode-map))

(define-generic-mode online-judge-test-mode
  '("[*]"
    "[!]"
    "[x]")
  nil
  '(("\\[-\\] .*$" . font-lock-warning-face)
    ("\\[\\+\\] .*$" . font-lock-constant-face))
  nil
  '(online-judge--test-outline-mode online-judge--set-keys)
  nil)

(put 'online-judge-test-mode 'mode-class 'special)



(defun online-judge--sample-format ()
  ""
  (or online-judge--sample-format
   (setq online-judge--sample-format
        (if online-judge-1-directory-p
            (format "%s-%s" (file-name-base (buffer-file-name))
                    online-judge--base-sample-format)
          online-judge--base-sample-format))))

(defun online-judge--command-name ()
  ""
  (or (eval online-judge-command-name)
      (format
       (if (eq system-type 'windows-nt) "%s.exe" "%s")
       (file-name-base (buffer-file-name)))))

(defun online-judge--add-buffer-separator-and-hide (str)
  ""
  (with-current-buffer (get-buffer-create online-judge--buffer-name)
    (online-judge-test-mode)
    (goto-char (point-max))
    (insert "\n# " (format-time-string "%F %R:%S") " " str "\n")
    (outline-hide-body)))

(defun online-judge--run-oj (&rest args)
  ""
  (online-judge--add-buffer-separator-and-hide (car args))
  (eval
   `(start-process
     "online-judge" ,online-judge--buffer-name ,online-judge-executable
     ,@args)))

(defun online-judge--run-oj-sync (&rest args)
  ""
  (online-judge--add-buffer-separator-and-hide (car args))
  (eval
   `(call-process
     ,online-judge-executable nil ,online-judge--buffer-name nil
     ,@args)))

(defun online-judge--make-oj-command (&rest args)
  ""
  (mapconcat #'shell-quote-argument
            (cons online-judge-executable args)
            " "))

(defun online-judge--oj-sequential-command (&rest args)
  ""
  (mapconcat
   (lambda (arg) (apply #'online-judge--make-oj-command arg))
   args " || "))

(defun online-judge--sequential-command (&rest args)
  ""
  (mapconcat #'identity args " || "))



(defun online-judge--command-submit ()
  ""
  (list "submit" (online-judge--get-url) (buffer-file-name) "--yes"))

(defun online-judge--command-download-test ()
  ""
  (list "download" (online-judge--get-url)
        "--format" (online-judge--sample-format)))

(defun online-judge--command-run-test ()
  ""
  (list "test"
        "--format"
        (if (string-match "%i"
                          (online-judge--sample-format))
            (replace-match "%s" nil nil (online-judge--sample-format))
          "%s.%e")
        "--command" (online-judge--command-name)
        "--print-input" "-e" (prin1-to-string online-judge--error-range)))



(defun online-judge--completing-host ()
  ""
  (intern
   (completing-read
    (format "Select host (default: %s): "
            online-judge-default-host)
    online-judge--host-alist
    nil t nil nil (symbol-name online-judge-default-host))))

(defun online-judge--completing-host-cons ()
  ""
  (assq (online-judge--completing-host) online-judge--host-alist))

(defun online-judge--set-all ()
  ""
  (unless online-judge--set
   (let* ((path (expand-file-name (file-name-sans-extension (buffer-file-name))))
         (all-dirs (split-string path "/"))
         index
         (host-cons
          (or
           (-find (lambda (arg)
                    (setq
                     index
                     (-find-last-index
                      (lambda (dir)
                        (string-match (plist-get (cdr arg) :host) dir))
                      all-dirs)))
                  online-judge--host-alist)
           ;; when host is not found
           (if online-judge-ask-when-unconfirmed
               (setq host-cons (online-judge--completing-host-cons))
             (message "Could not find host. Use %S." online-judge-default-host)
             (setq host-cons
                   (assq online-judge-default-host online-judge--host-alist)))))
         (plist (cdr host-cons))
         (contest-exact-regexp (plist-get plist :contest-exact))
         (contest-regexp       (plist-get plist :contest))
         (problem-regexp       (plist-get plist :problem))
         (dirs (nthcdr (or index 0) all-dirs))
         contest-index contest problem-index problem)
    ;; Find exact contest name.
    (setq
     contest-index
     (-find-last-index (lambda (dir)
                         (string-match contest-exact-regexp dir))
                       dirs))
    (when contest-index
      (let ((matched-string (nth contest-index dirs)))
       (string-match contest-exact-regexp matched-string)
      (setq contest (match-string 0 matched-string))
      (setf (nth contest-index dirs)
            (replace-match "/" nil nil matched-string))))

    ;; Find string that may be contest name.
    ;; Ask contest name to user whether it's found or not.
    (unless contest
      (setq contest-index
            (-find-last-index (lambda (dir)
                         (string-match contest-regexp dir))
                              dirs))
      (let ((matched-string (and contest-index (nth contest-index dirs)))
            maybe-contest)
       (when matched-string
         (string-match contest-regexp matched-string)
         (setq maybe-contest (match-string 0 matched-string)))
      (setq contest
            (read-from-minibuffer "contest name: " maybe-contest))
      (when (equal contest maybe-contest)
        (setf (nth contest-index dirs)
              (replace-match "/" nil nil matched-string)))))

    ;; Find problem
    (setq problem-index
     (-find-last-index (lambda (dir)
                         (string-match problem-regexp dir))
                       dirs))
    (setq problem
          (or
           (and problem-index
                (string-match problem-regexp (nth problem-index dirs))
                (match-string 0 (nth problem-index dirs)))
           (read-from-minibuffer "Problem ID: ")))

    (setq online-judge--host (car host-cons))
    (setq online-judge--contest contest)
    (setq online-judge--problem problem)
    (setq online-judge--set t))))

(defun online-judge--get-url ()
  ""
  (or online-judge--this-url
      (eval
       (plist-get
        (cdr (assq (online-judge--get-host) online-judge--host-alist)) :url))))

(defun online-judge--get-host ()
  ""
  (online-judge--set-all)
  online-judge--host)

(defun online-judge--get-contest ()
  ""
  (online-judge--set-all)
  online-judge--contest)

(defun online-judge--get-problem ()
  ""
  (or online-judge--problem
      (let (problem)
       (setq online-judge--problem problem))))

(defun online-judge--mode-line ()
  ""
  (propertize
   (format "%s%s%s"
           (if (eq online-judge-display-mode-line t)
               (concat
                (symbol-name online-judge--host)
                online-judge-separator-1)
             "")
           (if (or (eq online-judge-display-mode-line t)
                   (eq online-judge-display-mode-line contest))
            (concat
            online-judge--contest
            online-judge-separator-2)
            "")
           (when online-judge-display-mode-line
             online-judge--problem))
   'bold t))



(define-minor-mode online-judge-mode
  "Online judge mode, to use oj in Emacs."
  nil "oj" (make-sparse-keymap)
  :group 'online-judge
  (when online-judge-mode (online-judge--set-all))
  (if online-judge-mode
      (add-to-list 'mode-line-format online-judge-mode-line-list t)
    (delete online-judge-mode-line-list mode-line-format)))

(defun online-judge-download-test (&optional next)
  ""
  (interactive)
  (if online-judge--test-downloading
      (set-process-sentinel
       online-judge--test-downloading
       `(lambda (process string)
          (funcall
           ,(or (process-sentinel online-judge--test-downloading) #'ignore)
           process string)
          ,next))
    (set-process-sentinel
     (setq online-judge--test-downloading
           (apply
            #'online-judge--run-oj
            (online-judge--command-download-test)))
     (if next
         `(lambda (process string)
            (with-current-buffer ,(current-buffer)
              (if (or (string= string "finished\n")
                      online-judge--test-downloaded)
                  (progn
                    (setq online-judge--test-downloaded t)
                    ,next)
                (message "Download failed."))
              (setq online-judge--test-downloading nil)))
       (lambda (process string)
         (with-current-buffer ,(current-buffer)
           (when (string= string "finished\n")
             (setq online-judge--test-downloaded t))
           (message "Download %s."
                    (cond
                     ((string= string "finished\n") "succeeded")
                      (online-judge--test-downloaded "have already finished")
                      (t "failed")))
           (setq online-judge--test-downloading nil)))))))

(defun online-judge-run-test (&optional next)
  "Run test."
  (interactive)
  (if (not online-judge-mode) (error "online-judge-mode is off")
    (online-judge-download-test
     `(set-process-sentinel
       (apply #'online-judge--run-oj (online-judge--command-run-test))
       (lambda (process string)
         (if (string= "finished\n" string)
             ,(if next
                  `(with-current-buffer ,(current-buffer) ,next)
                '(display-buffer online-judge--buffer-name))
           (message "Test Failed. ")
           (display-buffer online-judge--buffer-name)))))))

(defun online-judge-submit ()
  ""
  (interactive)
  (if (not online-judge-mode) (error "online-judge-mode is off")
    (save-buffer)
    (let ((process (apply
                    #'online-judge--run-oj
                    (online-judge--command-submit))))
      (set-process-sentinel
       process (lambda (_process string)
                 (message "Submission %s."
                  (if (string= "finished\n" string)
                      "succeeded" "failed")))))))

(defun online-judge-test&submit ()
  ""
  (interactive)
  (online-judge-run-test '(online-judge-submit)))

(defun online-judge-toggle-error-range ()
  ""
  (interactive)
  (if (= 0 online-judge--error-range)
      (setq online-judge--error-range (or online-judge--error-range-before
                        online-judge-error-range-default))
    (setq online-judge--error-range-before online-judge--error-range)
    (setq online-judge--error-range 0))
  (message "Error range is set to %f." online-judge--error-range))

(defun online-judge-update-error-range (error)
  ""
  (interactive "nError range: ")
  (setq online-judge--error-range error)
  (message "Error range is set to %f." online-judge--error-range))

(defun online-judge-login ()
  ""
  (interactive)
  (let* ((host-cons (online-judge--completing-host-cons))
         (home (plist-get (cdr host-cons) :home))
         state)
    (while (not (and state (= state 0)))
      (let ((username (read-from-minibuffer
                       (concat
                        (when state "Login failed. Try again. ")
                        "Username: ")))
            (password (read-passwd "Password: ")))
        (setq state (online-judge--run-oj-sync
                     "login" home
                     "-u" username "-p" password))))
    (setcdr (assq (car host-cons) online-judge--login-alist) t))
  (message "Login succeeded."))

(defun online-judge-set-url (url)
  ""
  (interactive "sURL: ")
  (setq online-judge--this-url url))



(defun online-judge--atcoder-url ()
  "Get URL on AtCoder."
  (cond
   ((string-match "\\(past[0-9]+\\)\\(-open\\)?" (online-judge--get-contest))
    (format
     "https://atcoder.jp/contests/%s/tasks/%s_%s"
     (concat (match-string 1 (online-judge--get-contest)) "-open")
     (match-string 1 (online-judge--get-contest))
     (online-judge--get-problem)))
   ((string-match "\\([Cc]hokudai\\|CHOKUDAI\\)001" (online-judge--get-contest))
    "https://atcoder.jp/contests/chokudai001/tasks/chokudai_001_a")
   (t
    (format
     "https://atcoder.jp/contests/%s/tasks/%s_%s"
     (downcase (online-judge--get-contest))
     (downcase (online-judge--get-contest))
     (cond
      ((or (and
             (string-match "[Aa][Rr][Cc]\\([0-9]+\\)"
                           (online-judge--get-contest))
             (> 35 (string-to-number
                    (match-string 1 (online-judge--get-contest)))))
           (and
             (string-match "[Aa][Bb][Cc]\\([0-9]+\\)"
                           (online-judge--get-contest))
             (> 20 (string-to-number
                    (match-string 1 (online-judge--get-contest))))))
       (format
        "%s"
        (1+ (- (elt (downcase (online-judge--get-problem)) 0)
               ?a))))
      (t (downcase (online-judge--get-problem)))))))
  )



(defun online-judge--mode-on-if-matched ()
  ""
  (when (--any (f-ancestor-of? it (buffer-file-name)) online-judge-directories)
    (online-judge-mode +1)))

(add-hook 'find-file-hook 'online-judge--mode-on-if-matched)

(provide 'online-judge)
;;; online-judge.el ends here
