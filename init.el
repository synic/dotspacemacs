;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/private")
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     swift
     csv
     lua
     nginx
     sql
     auto-completion
     emacs-lisp
     git
     yaml
     javascript
     html
     markdown
     extra-langs
     python
     org
     osx
     python
     ipython-notebook
     django
     syntax-checking
     spell-checking
     colors
     spacemacs-layouts
     vimscript
     helm
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
                                      dockerfile-mode
                                      restclient
                                      zenburn-theme
                                      darktooth-theme
                                      gruvbox-theme
                                      wakatime-mode
                                      yasnippet-snippets
                                      )
   ;; A list of packages that cannot be updated.
   dotpsacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(smartparens
                                    wolfram-mode
                                    window-purpose)
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https nil
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5
   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default nil)
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 1
   ;; List of items to show in startup buffer or an association list of of
   ;; the form `(list-type . list-size)`. If nil it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         jbeans
                         zenburn
                         spacemacs-dark
                         spacemacs-light
                         darktooth
                         gruvbox
                         )
   ;; if non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
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
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key ":"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ t
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, `J' and `K' move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
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
   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 9999
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non-nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non-nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non-nil, the paste transient-state is enabled. While enabled, pressing
   ;; `p' several times cycles through the elements in the `kill-ring'.
   ;; (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil
   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non-nil the frame is maximized when Emacs starts up.
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
   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers t
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
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
   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"
   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'all
   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil
   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil
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
  ;; disable .zshrc env warnings, they aren't your mom
  (setq exec-path-from-shell-check-startup-files nil)

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
you should place your code here."
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
  (setenv "GOPATH" "/Users/adam.olsen/Projects/go")
  (setenv "PATH" (concat
                  "/Users/adam.olsen/Projects/go/bin" ":"
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
  (setq-default evil-escape-key-sequence "jk"
                evil-escape-delay .2)

  ;; Add `~/.spacepacs.d/private/snippets/' as a path to look for snippets for
  ;; yasnippets
  (setq private-yas-dir
        (expand-file-name "~/.spacemacs.d/private/snippets"))

  (add-hook 'yas-before-expand-snippet-hook
            (lambda ()
              (add-to-list 'yas-snippet-dirs private-yas-dir)))

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
              (setq fill-column 79)
              ;; Set tab-width to 4
              (setq tab-width 4
                    evil-shift-width 4)
              ;; Enable automatic line wrapping at fill column
              (auto-fill-mode t)))

  ;; ReST
  (add-hook `rst-mode-hook
            (lambda ()
              (setq fill-column 79
                    tab-width 3
                    evil-shift-width 3)
              (auto-fill-mode t)))

  ;; YAML hooks
  (add-hook 'yaml-mode-hook
            (lambda ()
                (fci-mode t)
                (auto-fill-mode t)
                (linum-mode t)
                (setq fill-column 79
                      tab-width 2
                      evil-shift-width 2)))

  ;; Markdown
  (add-hook 'markdown-mode-hook
            (lambda ()
              (fci-mode t)
              (auto-fill-mode t)
              (linum-mode t)
              (setq fill-column 79
                    tab-width 2
                    evil-shift-width 2)))

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
              (setq fill-column 79
                    tab-width 2
                    evil-shift-width 2)))

  ;; css
  (add-hook 'css-mode-hook
            (lambda ()
              (setq tab-width 2
                    fill-column 79
                    evil-shift-width 2)))

  ;; web (html)
  (add-hook 'web-mode-hook
            (lambda ()
              (setq fill-column 79
                    tab-width 2
                    evil-shift-width 2
                    web-mode-css-indent-offset 2
                    web-mode-code-indent-offset 2
                    web-mode-markup-indent-offset 2)))

  ;; javascript
  (setq-default js2-basic-offset 2
                js-indent-level 2)

  ;; disable trailing comma warning, it's 2017 gosh darnit
  (setq js2-strict-trailing-comma-warning nil)

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
                                 "\\|^\\..+$\\|\\.pyc$\\|^__pycache__$"))
  (add-hook 'dired-mode-hook 'ao/dired-omit-caller)
  (define-key evil-normal-state-map (kbd "_") 'projectile-dired)
  (define-key evil-normal-state-map (kbd "-") 'dired-jump)
  (setq diredp-hide-details-initially-flag nil)
  ;; Make `gg' and `G' do the correct thing
  (with-eval-after-load 'dired
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
  (setq helm-ag-base-command
        "ag --nocolor --nogroup --cc --csharp --python --cpp --go --java --lisp")

  ;; Make :enew work
  (defalias 'enew 'spacemacs/new-empty-buffer)

  ;; fix helm-bookmark-map
  (require 'helm-bookmark)

  ;; Bind up user functions
  (evil-leader/set-key "ow" 'ao/what-face)
  (evil-leader/set-key "ob" 'ao/show-file-name)
  (evil-leader/set-key "oa" 'avy-goto-char-2)

  ;; Map avy to `SPC SPC', where it should be ;-)
  (evil-leader/set-key "SPC" 'evil-avy-goto-word-or-subword-1)

  ;; Map avy window to `SPC w SPC', where it should be ;-)
  (evil-leader/set-key "w <SPC>" 'ace-window)

  ;; Update diff-hl on the fly
  (diff-hl-flydiff-mode)

  ;; disable stupid tag notifications
  (setq tags-add-tables nil)

  ;; format linum correctly
  (setq linum-format "%3i ")

  ;; open gists automatically after creating them
  (setq gist-view-gist t)

  ;; enable wakatime
  (global-wakatime-mode)

  ;; disable lock files because they screw up the ember server
  (setq create-lockfiles nil)

  ;; Disable smartparents toggles in web-mode, because they screw up formatting
  ;; for django template variables.  Also re-enables web-mode's default
  ;; auto-pairing
  ;; (add-hook 'web-mode-hook 'spacemacs/toggle-smartparens-off 'append)
  ;; (setq web-mode-enable-auto-pairing t)
  )

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil t)
 '(ahs-default-range (quote ahs-range-whole-buffer) t)
 '(ahs-idle-interval 0.25 t)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil t)
 '(diredp-hide-details-initially-flag nil)
 '(evil-escape-mode t)
 '(evil-shift-width 4)
 '(evil-want-Y-yank-to-eol t)
 '(fci-rule-color "#383838" t)
 '(linum-format " %5i ")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (gruvbox-theme vala-snippets vala-mode symon pkgbuild-mode org-brain logcat kivy-mode impatient-mode hoon-mode evil-org ebuild-mode cmake-ide levenshtein evil-lion browse-at-remote sparql-mode password-generator editorconfig realgud test-simple loc-changes load-relative magithub ghub+ apiwrap ghub company-lua wakatime-mode winum jbeans-theme ein string-inflection csv-mode autothemer hide-comnt helm-purpose window-purpose imenu-list lua-mode nginx-mode pug-mode origami vimish-fold wgrep smex ivy-hydra flyspell-correct-ivy counsel-projectile counsel swiper ivy restclient osx-dictionary py-isort dumb-jump powerline spinner org alert log4e gntp markdown-mode skewer-mode simple-httpd json-snatcher json-reformat multiple-cursors js2-mode hydra parent-mode projectile request haml-mode gitignore-mode fringe-helper git-gutter+ git-gutter gh marshal logito pcache ht flyspell-correct flycheck pkg-info epl flx magit magit-popup git-commit with-editor smartparens iedit anzu evil goto-chg undo-tree highlight diminish web-completion-data tern pos-tip company bind-map bind-key yasnippet packed anaconda-mode pythonic f s helm avy helm-core async auto-complete popup package-build dash-functional dash zenburn-theme yapfify yaml-mode xterm-color ws-butler wolfram-mode window-numbering which-key web-mode web-beautify volatile-highlights vimrc-mode vi-tilde-fringe uuidgen use-package toc-org thrift tagedit stan-mode sql-indent spacemacs-theme spaceline smeargle slim-mode shell-pop scss-mode scad-mode sass-mode reveal-in-osx-finder restart-emacs rainbow-mode rainbow-identifiers rainbow-delimiters quelpa qml-mode pyvenv pytest pyenv-mode popwin pony-mode pip-requirements persp-mode pcre2el pbcopy paradox osx-trash orgit org-projectile org-present org-pomodoro org-plus-contrib org-download org-bullets open-junk-file neotree multi-term move-text mmm-mode matlab-mode markdown-toc magit-gitflow magit-gh-pulls macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode launchctl julia-mode json-mode js2-refactor js-doc jade-mode info+ indent-guide ido-vertical-mode hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag google-translate golden-ratio gnuplot github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md flyspell-correct-helm flycheck-pos-tip flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav dockerfile-mode disaster dired+ diff-hl define-word dactyl-mode cython-mode company-web company-tern company-statistics company-quickhelp company-c-headers company-anaconda column-enforce-mode color-identifiers-mode coffee-mode cmake-mode clean-aindent-mode clang-format bracketed-paste auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile arduino-mode aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(paradox-github-token t)
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(ring-bell-function (quote ignore))
 '(safe-local-variable-values
   (quote
    ((eval when
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1))
     (python-shell-virtualenv-root . "/Users/adam.olsen/.virtualenvs/skedup")
     (python-shell-virtualenv-root . "/Users/adam.olsen/.virtualenvs/eventboard.io")
     (python-shell-virtualenv-path . "/Users/adam.olsen/.virtualenvs/skedup")
     (eval when
           (require
            (quote rainbow-mode)
            nil t)
           (rainbow-mode 1))
     (python-shell-virtualenv-path . "/Users/adam.olsen/.virtualenvs/eventboard.io"))))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit aw-mode-line-face :height 1.2))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yasnippet-snippets iedit org-mime bind-key highlight goto-chg flyspell-correct epl ghub let-alist pythonic alert dash-functional f org-category-capture projectile request-deferred websocket packed avy anaconda-mode tern smartparens magit magit-popup evil flycheck git-commit with-editor company helm helm-core async yasnippet multiple-cursors skewer-mode js2-mode simple-httpd markdown-mode org-plus-contrib hydra haml-mode powerline dash s swift-mode zenburn-theme yapfify yaml-mode xterm-color ws-butler wolfram-mode winum which-key web-mode web-beautify wakatime-mode volatile-highlights vimrc-mode vi-tilde-fringe uuidgen use-package toc-org thrift tagedit stan-mode sql-indent spaceline smeargle slim-mode shell-pop scss-mode scad-mode sass-mode reveal-in-osx-finder restclient restart-emacs rainbow-mode rainbow-identifiers rainbow-delimiters qml-mode pyvenv pytest pyenv-mode py-isort pug-mode popwin pony-mode pip-requirements persp-mode pcre2el pbcopy paradox osx-trash osx-dictionary orgit org-projectile org-present org-pomodoro org-download org-bullets open-junk-file nginx-mode neotree multi-term move-text mmm-mode matlab-mode markdown-toc magit-gitflow macrostep lua-mode lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode launchctl julia-mode json-mode js2-refactor js-doc jbeans-theme info+ indent-guide hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag gruvbox-theme google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md fuzzy flyspell-correct-helm flycheck-pos-tip flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav ein dumb-jump dockerfile-mode disaster dired+ diff-hl darktooth-theme dactyl-mode cython-mode csv-mode company-web company-tern company-statistics company-c-headers company-anaconda column-enforce-mode color-identifiers-mode coffee-mode cmake-mode clean-aindent-mode clang-format auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile arduino-mode aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(safe-local-variable-values
   (quote
    ((encoding . utf-8)
     (eval when
           (require
            (quote rainbow-mode)
            nil t)
           (rainbow-mode 1))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:foreground "#cccccc" :background "#151515")))))
