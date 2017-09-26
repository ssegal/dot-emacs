;;; -*- lexical-binding: t -*-
;;
;; Portions adopted from https://github.com/habnabit/remote-emacsclient

(require 'tramp)
(require 'tramp-sh)

(defgroup rclient nil
  "Allow remote clients to connect to a running Emacs server."
  :group 'tramp
  :group 'server
  :version "25.1")

(defcustom rclient-default-remote-auth-file "~/.emacs.d/remote-server"
  "Default remote path at which to save the remote emacsclient
authentication file. This can be a string or nil to disable
saving an authentication file.

The authentication file is similar to the one written out by the
emacsclient TCP server, except it includes the prefix used for
the TRAMP connection to the remote server."
  :group 'rclient
  :type '(choice (const nil) string))

(defcustom rclient-default-remote-socket-path "~/.emacs.d/remote-server-socket"
  "Default remote Unix-domain socket path that will be forwarded
to the local server."
  :group 'rclient
  :type 'string)

(defcustom rclient-remote-auth-file-alist nil
  "The remote emacsclient authentication file path to use for
specific host/user pairs. This is an alist of items (HOST USER
PATH). The first matching item specifies the path to use for a
connection which does not specify a method. HOST and USER are
regular expressions or nil, which is interpreted as a regular
expression which always matches. If no entry matches, the
variable `rclient-default-remote-auth-file' takes
effect.

If the connection does not specify the user, lookup is done using
the empty string for the user name.

See `rclient-default-remote-auth-file' for an
explanation of the auth file path."
  :group 'rclient
  :type '(repeat (list (choice :tag "Host regexp" regexp (const nil))
		       (choice :tag "User regexp" regexp (const nil))
		       (choice :tag "emacsclient auth path" string (const nil)))))

(defcustom rclient-remote-socket-path-alist nil
  "The remote server socket path to use for specific host/user
pairs. This is an alist of items (HOST USER PATH). The first
matching item specifies the path to use for a connection which
does not specify a method. HOST and USER are regular expressions
or nil, which is interpreted as a regular expression which always
matches. If no entry matches, the variable
`rclient-default-remote-socket-path' takes effect.

If the connection does not specify the user, lookup is done using
the empty string for the user name.

See `rclient-default-remote-socket-path' for an
explanation of the socket path."
  :group 'rclient
  :type '(repeat (list (choice :tag "Host regexp" regexp (const nil))
		       (choice :tag "User regexp" regexp (const nil))
		       (choice :tag "socket path" string (const nil)))))

(defun rclient--make-tramp-file-name-from-vec (vec file)
  "Convenience function for making a TRAMP path, since this
apparently didn't already exist."
  (tramp-make-tramp-file-name
    (tramp-file-name-method vec)
    (tramp-file-name-user vec)
    (tramp-file-name-host vec)
    file))

(defun rclient--canonicalize-remote-path (vec path)
  (with-current-buffer (tramp-get-buffer vec)
    (s-trim (shell-command-to-string (format "echo %s" path)))))

(defun rclient-get-remote-auth-file (vec)
  "Determine the path for the remote auth file, given a
connection vector."
  (let ((choices rclient-remote-auth-file-alist)
        (host (or (tramp-file-name-host vec) ""))
        (user (or (tramp-file-name-user vec) ""))
        lfile item matched)
    (while choices
      (setq item (pop choices))
      (when (and (string-match (or (nth 0 item) "") host)
                 (string-match (or (nth 1 item) "") user))
        (setq lfile (nth 2 item)
              choices nil
                    matched t)))
    (if matched lfile rclient-default-remote-auth-file)))

(defun rclient-get-remote-socket-path (vec)
  "Determine the path for the remote socket path, given a
connection vector."
  (let ((choices rclient-remote-socket-path-alist)
        (host (or (tramp-file-name-host vec) ""))
        (user (or (tramp-file-name-user vec) ""))
        lfile item matched)
    (while choices
      (setq item (pop choices))
      (when (and (string-match (or (nth 0 item) "") host)
                 (string-match (or (nth 1 item) "") user))
        (setq lfile (nth 2 item)
              choices nil
                    matched t)))
    (if matched lfile rclient-default-remote-socket-path)))

(defun rclient--save-remote-auth-file (vec)
  (condition-case err
      (let ((auth-file (rclient-get-remote-auth-file vec))
            (conn-type (process-contact server-process :family))
            (auth-key (process-get server-process :auth-key))
            (port (process-contact server-process :service))
            (out-alist nil))
        (if auth-file
            (with-temp-file (rclient--make-tramp-file-name-from-vec vec auth-file)
              (when auth-key
                (push (cons 'auth-key (process-get server-process :auth-key)) out-alist))
              (cond ((eq conn-type 'ipv4)
                     (push (cons 'address (list "127.0.0.1" port)) out-alist))
                    ((eq conn-type 'ipv6)
                     (push (cons 'address (list "::1" port)) out-alist))
                    ((eq conn-type 'local)
                     (push (cons 'address
                                 (rclient--canonicalize-remote-path
                                  vec (rclient-get-remote-socket-path vec))) out-alist))
                    (t (error "Unknown server connection type")))
              (push (cons 'tramp-prefix (server-quote-arg (rclient--make-tramp-file-name-from-vec vec ""))) out-alist)
              (insert (json-encode out-alist)))))
    (file-error (message "error saving remote emacsclient auth: %s" err))))

(defun rclient-configure-remote-client (&optional vec)
  (interactive)
  (let ((vec (or vec (tramp-dissect-file-name default-directory))))
    (when (not (and tramp-use-ssh-controlmaster-options
                    (equal (tramp-get-method-parameter vec 'tramp-login-program) "ssh")))
      (error "Remote client only supported on SSH-based connections with ControlMaster"))
    (rclient--save-remote-auth-file vec)
    (let* ((default-directory temporary-file-directory)
           (server-socket (process-contact server-process :local))
           (remote-socket (rclient--canonicalize-remote-path
                           vec (rclient-get-remote-socket-path vec)))
           (port (process-contact server-process :service))
           (cmd (if (stringp server-socket)
                    (progn
                      (delete-file (rclient--make-tramp-file-name-from-vec vec remote-socket))
                      (format "ssh -O forward %s -R%s:%s %s%s"
                              (format (tramp-ssh-controlmaster-options vec))
                              remote-socket
                              server-socket
                              (if (tramp-file-name-user vec)
                                  (concat (tramp-file-name-user vec) "@")
                                "")
                              (tramp-file-name-host vec)))
                  (format "ssh -O forward %s -R%d:127.0.0.1:%d %s%s"
                          (format (tramp-ssh-controlmaster-options vec))
                          port port
                          (if (tramp-file-name-user vec)
                              (concat (tramp-file-name-user vec) "@")
                            "")
                          (tramp-file-name-host vec)))))
      (process-file-shell-command cmd))))

(advice-add 'tramp-open-connection-setup-interactive-shell :after
            (lambda (proc vec)
              (when (and (string-match-p "^*tramp.**" (process-name proc))
                         tramp-use-ssh-controlmaster-options
                         (equal (tramp-get-method-parameter vec 'tramp-login-program) "ssh")
                         server-process
                         (stringp (process-contact server-process :local)))
                (rclient-configure-remote-client vec)))
            '((name rclient-initialize-connection)))

(provide 'rclient)
