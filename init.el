;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '(
                                           "~/.spacemacs.d/private")
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     go
     sql
     nginx
     unscroll
     auto-completion
     xkcd
     emacs-lisp
     git
     yaml
     github
     javascript
     html
     markdown
     extra-langs
     python
     ruby
     org
     osx
     python
     django
     go
     syntax-checking
     spell-checking
     lua
     colors
     spacemacs-layouts
     vimscript
     spacemacs-helm
     (version-control :variables
                      version-control-diff-tool 'diff-hl)
     (c-c++ :variables
            c-c++-enable-clang-support t)
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(dired+
                                      evil-visual-mark-mode
                                      dockerfile-mode
                                      zenburn-theme
                                     )
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(
                                    flycheck-pos-tip)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https nil
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update t
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 1
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         jbeans
                         spacemacs-dark
                         spacemacs-light
                         )
   ;; if non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Hack"
                               :size 9
                               :weight normal
                               :width normal
                               :powerline-scale 1.2)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key ":"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ t
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "D"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout t
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title nil
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide nil
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers t
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'all
   ))

(defvar ao/v-dired-omit t
  "If dired-omit-mode enabled by default. Don't setq me.")

(defun ao/dired-omit-switch ()
  "This function is a small enhancement for `dired-omit-mode', which will
   \"remember\" omit state across Dired buffers."
  (interactive)
  (if (eq ao/v-dired-omit t)
      (setq ao/v-dired-omit nil)
    (setq ao/v-dired-omit t))
  (ao/dired-omit-caller)
  (when (equal major-mode 'dired-mode)
    (revert-buffer)))

(defun ao/dired-omit-caller ()
  (if ao/v-dired-omit
      (setq dired-omit-mode t)
    (setq dired-omit-mode nil)))

(defun ao/dired-back-to-top()
  "Move to the first file."
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 2))

(defun ao/dired-jump-to-bottom()
  "Move to last file."
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(defun ao/what-face (pos)
  "Describes the face at the current cursor position.
Helps making themes, put your cursor at the point you want to know the face of, and
M-x ao/what-face."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun ao/show-file-name ()
  "Show the full path of the current buffer."
  (interactive)
  (message (buffer-file-name)))

(defun save-framegeometry ()
  "Gets the current frame's geometry and saves to ~/.emacs.d/framegeometry."
  (let (
        (framegeometry-left (frame-parameter (selected-frame) 'left))
        (framegeometry-top (frame-parameter (selected-frame) 'top))
        (framegeometry-width (frame-parameter (selected-frame) 'width))
        (framegeometry-height (frame-parameter (selected-frame) 'height))
        (framegeometry-file (expand-file-name "~/.emacs.d/framegeometry"))
        )

    (when (not (number-or-marker-p framegeometry-left))
      (setq framegeometry-left 0))
    (when (not (number-or-marker-p framegeometry-top))
      (setq framegeometry-top 0))
    (when (not (number-or-marker-p framegeometry-width))
      (setq framegeometry-width 0))
    (when (not (number-or-marker-p framegeometry-height))
      (setq framegeometry-height 0))

    (with-temp-buffer
      (insert
       ";;; This is the previous emacs frame's geometry.\n"
       ";;; Last generated " (current-time-string) ".\n"
       "(setq initial-frame-alist\n"
       "      '(\n"
       (format "        (top . %d)\n" (max framegeometry-top 0))
       (format "        (left . %d)\n" (max framegeometry-left 0))
       (format "        (width . %d)\n" (max framegeometry-width 0))
       (format "        (height . %d)))\n" (max framegeometry-height 0)))
      (when (file-writable-p framegeometry-file)
        (write-file framegeometry-file))))
  )

(defun load-framegeometry ()
  "Loads ~/.emacs.d/framegeometry which should load the previous frame's
geometry."
  (let ((framegeometry-file (expand-file-name "~/.emacs.d/framegeometry")))
    (when (file-readable-p framegeometry-file)
      (load-file framegeometry-file)))
  )

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  ;; Restore Frame size and location, if we are using gui emacs
  (if window-system
      (progn
        (add-hook 'after-init-hook 'load-framegeometry)
        (add-hook 'kill-emacs-hook 'save-framegeometry))
    )
  ;; Add `~/.emacs.d/libs' to the load-path, so that our custom libraries can be
  ;; found (specifically, `evil-vimish-fold' is not on melpa)
  (add-to-list 'load-path (expand-file-name  "~/.emacs.d/libs/"))

  ;; Add `~/.emacs.d/themes' to the theme load path, so that our custom themes
  ;; are loadable by placing them in `dotspacemacs-themes`
  (add-to-list 'custom-theme-load-path
        (expand-file-name "~/.spacemacs.d/private/themes"))
  (add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/")))

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place you code here."
  (setq
   ;; Don't hide details in dired by default
   diredp-hide-details-initially-flag nil
   ;; Set the fill column indicator color (can't be done in a theme for
   ;; some reason)
   fci-rule-color "#252525"
   ;; Use soft indent
   indent-tabs-mode nil
   ;; Don't show hidden files in neotree by default
   neo-show-hidden-files nil
   ;; Use nerdtree style theme for neotree.  Possible values are
   ;; 'classic, 'nerd, 'ascii, and 'arrow
   neo-theme 'nerd
   ;; Don't enable neotree vc-integration by default, as it is slow.
   neo-vc-integration nil
   ;; Projectile ignored suffices
   projectile-globally-ignored-file-suffixes '(".pyc" ".rst")
   ;; Enable web-mode engine detection
   web-mode-enable-engine-detection t
   ;; Make magit branch changes update the modeline
   auto-revert-check-vc-info nil
   ;; Set the default web-mode engine for .html files to "django"
   web-mode-engines-alist '(("django" . "\\.html\\'")))

  ;; set GOPATH for go autocompletion
  (setenv "GOPATH" "/Users/synic/Projects/go")
  (setenv "PATH" (concat
                  "/Users/synic/Projects/go/bin" ":"
                  (getenv "PATH")))

  ;; Disable active process prompt
  (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
    "Prevent annoying \"Active processes exist\" query when you quit Emacs."
    (flet ((process-list ())) ad-do-it))

  ;; Enable highlighting of git commit messages when emacs is $EDITOR
  (global-git-commit-mode t)

  ;; Make the cursor in non-focused windows a bar, so you can see the ace-window
  ;; leading char better
  (setq-default cursor-in-non-selected-windows 'bar)

  ;; set up evil escape
  (setq-default evil-escape-key-sequence "fd"
                evil-escape-delay .2)

  ;; Add `~/.spacepacs.d/private/snippets/' as a path to look for snippets for
  ;; yasnippets
  (setq private-snippet-dir
        (expand-file-name "~/.spacemacs.d/private/snippets"))
  (add-hook 'yas-before-expand-snippet-hook
            (lambda ()
              (add-to-list 'yas-snippet-dirs private-snippet-dir)))

  (add-hook 'hack-local-variables-hook
            (lambda ()
              (setq
               ;; Don't use virtual line wrapping by default
               truncate-lines t)))

  ;; Disable vi tilde in the fringe by default
  (global-vi-tilde-fringe-mode -1)

  ;; Python hooks
  (add-hook 'python-mode-hook
            (lambda ()
              ;; Enable fill column indicator
              (fci-mode t)
              ;; Turn on line numbering
              (linum-mode t)
              ;; Set tab-width to 4
              (setq tab-width 4)
              ;; Set evil-shift-width to 4
              (setq evil-shift-width 4)
              ;; Enable automatic line wrapping at fill column
              (auto-fill-mode t)))

  ;; YAML hooks
  (add-hook 'yaml-mode-hook
            (lambda ()
                (fci-mode t)
                (auto-fill-mode t)
                (linum-mode t)
                (setq fill-column 80
                      tab-width 2)))

  ;; Org
  (add-hook 'org-mode-hook
            (lambda ()
              ;; Enable fill column indicator
              (fci-mode t)
              ;; Turn off line numbering, it makes org so slow
              (linum-mode -1)
              ;; Set fill column to 79
              (setq fill-column 79)
              ;; Enable automatic line wrapping at fill column
              (auto-fill-mode t)))

  ;; Elisp
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (auto-fill-mode t)
              (fci-mode t)
              (linum-mode t)
              (setq fill-column 80
                    tab-width 2)))

  ;; Enable a blinking cursor
  (blink-cursor-mode t)

  ;; Magit settings
  ;; Fix `magit-blame-quit'
  (evil-leader/set-key "gB" 'magit-blame-quit)

  ;; Dired
  (require 'dired-x) ; Enable dired-x
  (require 'dired+)  ; Enable dired+
  (setq-default dired-omit-files-p t)  ; Don't show hidden files by default
  (setq dired-omit-files (concat dired-omit-files
                                 "\\|^\\..+$\\|\\.pyc$|^__pycache__$"))
  (add-hook 'dired-mode-hook 'ao/dired-omit-caller)
  (define-key evil-normal-state-map (kbd "_") 'projectile-dired)
  (define-key evil-normal-state-map (kbd "-") 'dired-jump)
  (setq diredp-hide-details-initially-flag nil)
  ;; Make `gg' and `G' do the correct thing
  (eval-after-load "dired-mode"
    (evilified-state-evilify dired-mode dired-mode-map
             [mouse-1] 'diredp-find-file-reuse-dir-buffer
             [mouse-2] 'dired-find-alternate-file
             "f"  'helm-find-files
             "h"  'diredp-up-directory-reuse-dir-buffer
             "l"  'diredp-find-file-reuse-dir-buffer
             "I"  'ao/dired-omit-switch
             "c"  'helm-find-files
             "gg" 'ao/dired-back-to-top
             "G"  'ao/dired-jump-to-bottom))

  ;; Bind SPC k ' to `ielm'
  (evil-leader/set-key "k'" 'ielm)

  ;; Bind `SPC t M' to evil-visual-mark-mode
  (require 'evil-visual-mark-mode)
  (evil-leader/set-key "tM" 'evil-visual-mark-mode)
  ;; Show marks like ^, [, ] in visual-mark-mode
  (setq evil-visual-mark-exclude-marks '())

  ;; `SPC w O' - close all the win
  ;; Same as "close other tabs" in chrome
  (evil-leader/set-key "wO" 'delete-other-windows)

  ;; helm-ag
  (setq helm-ag-use-agignore t)

  ;; Transparency by default
  (spacemacs/toggle-transparency)

  ;; Make :enew work
  (defalias 'enew 'spacemacs/new-empty-buffer)

  ;; Bind up user functions
  (evil-leader/set-key "ow" 'ao/what-face)
  (evil-leader/set-key "ob" 'ao/show-file-name)
  (evil-leader/set-key "oa" 'avy-goto-char-2)

  ;; Map avy to SPC SPC, where it should be ;-)
  (evil-leader/set-key "SPC" 'evil-avy-goto-word-or-subword-1)

  ;; Update diff-hl on the fly
  (diff-hl-flydiff-mode)

  ;; Disable smartparents toggles in web-mode, because they screw up formatting
  ;; for django template variables.  Also re-enables web-mode's default
  ;; auto-pairing
  (add-hook 'web-mode-hook 'spacemacs/toggle-smartparens-off 'append)
  (setq web-mode-enable-auto-pairing t)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(diredp-hide-details-initially-flag nil)
 '(evil-escape-mode t)
 '(evil-shift-width 4)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(paradox-github-token t)
 '(ring-bell-function (quote ignore) t)
 '(safe-local-variable-values
   (quote
    ((eval when
           (require
            (quote rainbow-mode)
            nil t)
           (rainbow-mode 1))
     (python-shell-virtualenv-path . "/Users/synic/.virtualenvs/eventboard.io")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :foundry "nil" :slant normal :weight normal :height 90 :width normal))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
