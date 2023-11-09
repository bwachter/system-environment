;; for string-trim-right, required for unclean outputs (Windows...)
(require 'subr-x)

(defvar system-environment-import-hook nil
  "Hooks running after execution of `system-environment-from-command'")
(defvar system-environment-import-async-hook nil
  "Hooks running in the sentinel for `system-environment-from-async-command'")

(defun system-environment-update-exec-path()
  "Loop over the PATH variable, and add each directory to exec-path

Emacs initialises the exec-path on startup from PATH, but afterwards they're
two separate things. If the imported variables also include PATH this most
likely should be called after import. The easiest way for doing so is hooking
it to the import functions:

(add-hook 'system-environment-import-hook 'system-environment-update-exec-path)
(add-hook 'system-environment-import-async-hook 'system-environment-update-exec-path)"
  (setq exec-path (remove-duplicates
                   (nconc exec-path (split-string (getenv "PATH") ":"))
                   :test 'string=)))

(defun system-environment-import-from-command(command &optional variable-names is-blacklist keep-buffer verbose)
  "Call a command dumping variables in key value pairs, and import them

This sets up a buffer suitable for `system-environment-import-from-buffer' -
most of this functions arguments as well as the expected output format are
documented there.

KEEP-BUFFER keeps the buffer around after finishing - which is mostly useful
for debugging.

Import systemd user session environment, with the exception of DISPLAY, HOME,
LOGNAME and USER:

(system-environment-import-from-command \"systemctl --user show-environment\" '(\"DISPLAY\" \"HOME\" \"LOGNAME\" \"USER\") t)))

Import PATH and SSH_AUTH_SOCK from zsh with verbose logging:

(system-environment-import-from-command \"zsh -i -c env\" '(\"PATH\" \"SSH_AUTH_SOCK\") nil nil t)
"
  (if (executable-find (car (split-string command)))
      (let ((buffer-name "*environment-import*"))
        (set-buffer (get-buffer-create buffer-name))
        (shell-command command
                       buffer-name
                       "*Messages*")
        (system-environment-import-from-buffer "*environment-import*" variable-names is-blacklist verbose)
        (run-hooks 'system-environment-import-hook)
        (when (not keep-buffer) (kill-buffer buffer-name)))
    (message (concat "Not importing environment, command not found: " command))))

(defun system-environment-import-from-async-command(command &optional variable-names is-blacklist keep-buffer verbose buffer)
  "Call an async command dumping variables in key value pairs, and import them

Some systems (*cough*Windows*cough*) are very slow when interacting with
external processes, so to avoid delaying startup it is better to asynchronously
handle the importing.

This sets up a buffer suitable for `system-environment-import-from-buffer' -
most of this functions arguments as well as the expected output format are
documented there.

KEEP-BUFFER keeps the buffer around after finishing - which is mostly useful
for debugging.

When calling this function more than once a custom BUFFER name should be
specified to make sure both calls dump into unique buffers.

(system-environment-import-from-async-command
 \"powershell -Command Get-ChildItem Env:|Foreach-Object {'{0}={1}' -f $_.Key,$_.Value }\")
"
  (if (executable-find (car (split-string command)))
      (let ((buffer-name (or buffer "*environment-import*"))
            (command-list (split-string command)))
        (push buffer-name command-list)
        (push (nth 1 command-list) command-list)
        (set-buffer (get-buffer-create buffer-name))
        (setq-local variable-names variable-names)
        (setq-local is-blacklist is-blacklist)
        (setq-local keep-buffer keep-buffer)
        (setq-local verbose verbose)
        (let ((process (apply 'start-process command-list)))
          (set-process-sentinel process 'system-environment-import-sentinel)))
    (message (concat "Not importing environment, command not found: " command))))

(defun system-environment-import-from-buffer(buffer-name &optional variable-names is-blacklist verbose)
  "Import environment variables in key=value format from buffer BUFFER-NAME.

To only import a subset of the variables a list of variable names can be
provided in VARIABLE-NAMES, with IS-BLACKLIST deciding if it is treated as
black- or whitelist.

With both VARIABLE-NAMES and IS-BLACKLIST nil all variables in the buffer will
imported.

With VARIABLE-NAMES set and IS-BLACKLIST nil (the default) variables will be
imported as long as they are on the list.

With VARIABLE-NAMES set and IS-BLACKLIST t (the default) variables on the list
will not be imported.

Setting VERBOSE to t will log additional information about the state of the
environment as well as what this function is doing to the *Messages* buffer.

For setting environments based on command output see the functions
`system-environment-import-from-command' and `system-environment-from-async-command'
"
  (set-buffer buffer-name)

  (goto-char 1)
  (while (not (eobp))
    (if (looking-at "\\(.*?\\)=\\(.*\\)")
        (progn
          (let* ((env-name (string-trim-right (match-string-no-properties 1)))
                 (env-value (string-trim-right (match-string-no-properties 2)))
                 (old-env-value (getenv env-name)))
            (when verbose
              (message "%S: %S" env-name env-value)
              (cond ((not old-env-value)
                     (message (concat "unset variable: " env-name)))
                    ((not (string= env-value old-env-value))
                     (message (concat "variable exists with different value: " env-name)))
                    ((string= env-value old-env-value)
                     (message (concat "variable exists with same value: " env-name)))
                    (t (message "should never happen"))))

                ;; check variable-names before setting variables
                (cond ((and is-blacklist (member env-name variable-names))
                       (when verbose (message (concat "blacklist and on list, skipping: " env-name))))
                      ((and (not is-blacklist) variable-names (not  (member env-name variable-names)))
                       (when verbose (message (concat "whitelist and not on list, skipping: " env-name))))
                      (t
                       (when verbose (message (concat "setting: " env-name)))
                       (setenv env-name env-value))
                      ))))
    (forward-line 1)))

(defun system-environment-import-sentinel (proc state)
  "This is the process sentinel for `system-environment-from-async-command'"
  (let ((buffer-name (process-buffer proc)))
    (set-buffer buffer-name)
    (system-environment-import-from-buffer buffer-name variable-names is-blacklist verbose)
    (run-hooks 'system-environment-import-async-hook)
    (when (not keep-buffer) (kill-buffer buffer-name))))

(provide 'system-environment)
