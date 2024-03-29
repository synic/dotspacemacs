;; -*- mode: emacs-lisp; lexical-binding: t -*-
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

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')

   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/private")

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(react
     protobuf
     dart
     ruby
     auto-completion
     colors
     csv
     dap
     django
     emacs-lisp
     git
     go
     helm
     html
     ipython-notebook
     javascript
     lua
     markdown
     major-modes
     nginx
     org
     osx
     perl5
     rust
     spacemacs-layouts
     spell-checking
     swift
     syntax-checking
     sql
     yaml
     vimscript

     (elm :variables
          elm-sort-imports-on-save t
          elm-format-on-save t)
     (python :variables
             python-backend 'lsp
             python-formatter 'black
             python-format-on-save t
             )

     (c-c++ :variables
            c-c++-enable-clang-support t)
     (lsp :variables
          lsp-headerline-breadcrumb-enable nil)
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     (typescript :variables
                 typescript-backend 'lsp
                 typescript-fmt-tool 'prettier
                 typescript-fmt-on-save t)
     (version-control :variables
                      version-control-diff-tool 'diff-hl)
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(dockerfile-mode
                                      rainbow-mode
                                      dired
                                      restclient
                                      jbeans-theme
                                      zenburn-theme
                                      darktooth-theme
                                      gruvbox-theme
                                      yasnippet-snippets
                                      handlebars-mode
                                      editorconfig
                                      graphql-mode
                                      vue-mode
                                      python-black
                                      )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(smartparens
                                    wolfram-mode
                                    treemacs-icons-dired
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
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update t

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

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

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

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

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key ":"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

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
   spacemacs-force-resume-layouts nil

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

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title nil

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   dotspacemacs-scroll-bar-while-scrolling nil

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
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
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

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

(defun ao/project-literal-search ()
  "Run `helm-do-ag-project-root' using a literal string."
  (interactive)
  (setq old-helm-ag-base-command helm-ag-base-command)
  (setq helm-ag-base-command (concat helm-ag-base-command " -Q"))
  (helm-do-ag-project-root)
  (setq helm-ag-base-command old-helm-ag-base-command))

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

  ;; enable projectile caching
  (setq projectile-enable-caching 1)

  ;; Restore Frame size and location, if we are using gui emacs
  (if window-system
      (progn
        (add-hook 'after-init-hook 'load-framegeometry)
        (add-hook 'kill-emacs-hook 'save-framegeometry))
    )
  ;; Add `~/.spacemacs.d/libs' to the load-path, so that our custom libraries can be
  ;; found (specifically, `dired+' is not on melpa)
  (add-to-list 'load-path (expand-file-name  "~/.spacemacs.d/libs/"))

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

  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

  ;; prevent getting into insert mode permanently (which ends up causing `d` to
  ;; act like `dd`.
  (defun kill-minibuffer ()
    (interactive)
    (when (windowp (active-minibuffer-window))
      (evil-ex-search-exit)))

  (add-hook 'mouse-leave-buffer-hook #'kill-minibuffer)

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
  (require 'git-commit)
  (global-git-commit-mode t)

  ;; Make the cursor in non-focused windows a bar, so you can see the ace-window
  ;; leading char better
  (setq-default cursor-in-non-selected-windows 'bar)

  ;; set up evil escape
  (setq-default evil-escape-key-sequence "fd"
                evil-escape-delay .2)

  (add-hook 'hack-local-variables-hook
            (lambda ()
              (setq
               ;; Don't use virtual line wrapping by default
               truncate-lines t)))

  ;; Disable vi tilde in the fringe by default
  (global-vi-tilde-fringe-mode -1)

  ;; Python hooks
  (setq-default lsp-pyls-configuration-sources ["flake8"])
  (setq-default lsp-pyls-plugins-pylint-enabled nil)

  (add-hook 'python-mode-hook
            (lambda ()
              ;; Enable fill column indicator
              ;; (display-fill-column-indicator-mode)
              (display-fill-column-indicator-mode)
              ;; Turn on line numbering
              ;; (linum-mode t)
              (setq fill-column 88)
              (setq-local lsp-diagnostics-provider :none)
              ;; set in-block indentation scale
              (setq python-indent-def-block-scale 1)
              ;; Set tab-width to 4
              (setq tab-width 4
                    evil-shift-width 4)
              ;; Enable automatic line wrapping at fill column
              (auto-fill-mode t)))

  ;; ReST
  (add-hook `rst-mode-hook
            (lambda ()
              (setq fill-column 88
                    tab-width 3
                    evil-shift-width 3)
              (auto-fill-mode t)))

  ;; YAML hooks
  (add-hook 'yaml-mode-hook
            (lambda ()
                (display-fill-column-indicator-mode)
                (auto-fill-mode t)
                ;; (linum-mode t)
                (setq fill-column 79
                      tab-width 2
                      evil-shift-width 2)))

  ;; Markdown
  (add-hook 'markdown-mode-hook
            (lambda ()
              (display-fill-column-indicator-mode)
              (auto-fill-mode t)
              ;; (linum-mode t)
              (setq fill-column 79
                    tab-width 2
                    evil-shift-width 2)))

  ;; Org
  (add-hook 'org-mode-hook
            (lambda ()
              ;; Enable fill column indicator
              (display-fill-column-indicator-mode)
              ;; Turn off line numbering, it makes org so slow
              ;; (linum-mode -1)
              ;; Set fill column to 79
              (setq fill-column 79)
              ;; Enable automatic line wrapping at fill column
              (auto-fill-mode t)))

  ;; Elisp
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (auto-fill-mode t)
              (display-fill-column-indicator-mode)
              ;; (linum-mode t)
              (setq fill-column 79
                    tab-width 2
                    evil-shift-width 2)))

  ;; css
  (add-hook 'css-mode-hook
            (lambda ()
              (display-fill-column-indicator-mode)
              (setq tab-width 2
                    fill-column 79
                    c-basic-offset 2
                    css-indent-offset 2
                    indent-tabs-mode nil
                    evil-shift-width 2)))

  ;; scss
  (add-hook 'scss-mode-hook
            (lambda ()
              (display-fill-column-indicator-mode)
              (setq tab-width 2
                    fill-column 79
                    c-basic-offset 2
                    css-indent-offset 2
                    indent-tabs-mode nil
                    evil-shift-width 2)))

  ;; js2
  (add-hook 'js2-mode-hook
            (lambda ()
              (display-fill-column-indicator-mode)
              (setq tab-width 2
                    fill-column 79
                    c-basic-offset 2
                    indent-tabs-mode nil
                    evil-shift-width 2)))

  ;; typescript
  (setq-default typescript-indent-level 2)
  (add-hook 'typescript-mode
            (lambda ()
              (display-fill-column-indicator-mode)
              (setq tab-width 2
                    c-basic-offset 2
                    typescript-indent-level 2
                    indent-tabs-mode nil
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

  (setq exec-path
        (append exec-path '("/home/synic/.nvm/versions/node/v16.10.0/bin")))

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
  (with-eval-after-load 'dired
    (evilified-state-evilify-map dired-mode-map
      :mode dired-mode
      :bindings
          [mouse-1] 'diredp-find-file-reuse-dir-buffer
          [mouse-2] 'dired-find-alternate-file
          "f"  'helm-find-files
          "r"  'dired-do-redisplay
          "h"  'diredp-up-directory-reuse-dir-buffer
          "l"  'diredp-find-file-reuse-dir-buffer
          "I"  'ao/dired-omit-switch
          "c"  'helm-find-files
          "gg" 'ao/dired-back-to-top
          "G"  'ao/dired-jump-to-bottom))

  (defun lsp-typescript-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'typescript-mode-hook #'lsp-typescript-install-save-hooks)

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
  (setq helm-ag-base-command "ag --nocolor --nogroup")

  ;; Make :enew work
  (defalias 'enew 'spacemacs/new-empty-buffer)

  ;; fix helm-bookmark-map
  (require 'helm-bookmark)

  ;; Bind up user functions
  ;; (evil-leader/set-key "ow" 'ao/what-face)
  ;; (evil-leader/set-key "ob" 'ao/show-file-name)
  ;; (evil-leader/set-key "oa" 'avy-goto-char-2)

  ;; ;; set `SPC sp' to project literal search
  ;; (evil-leader/set-key "sp" 'ao/project-literal-search)

  ;; ;; Map avy to `SPC SPC', where it should be ;-)
  ;; (evil-leader/set-key "SPC" 'evil-avy-goto-word-or-subword-1)

  ;; ;; Map avy window to `SPC w SPC', where it should be ;-)
  ;; (evil-leader/set-key "w <SPC>" 'ace-window)

  (spacemacs/declare-prefix "o" "own menu")
  (spacemacs/set-leader-keys
    "ow" 'ao/what-face
    "ob" 'ao/show-file-name
    "oa" 'avy-goto-char-2
    "SPC" 'avy-goto-word-or-subword-1
    "w <SPC>" 'ace-window
   )

  ;; Update diff-hl on the fly
  (diff-hl-flydiff-mode)

  ;; disable stupid tag notifications
  (setq tags-add-tables nil)

  ;; format linum correctly
  (setq linum-format "%3i ")

  ;; open gists automatically after creating them
  (setq gist-view-gist t)

  ;; disable lock files because they screw up the ember server
  (setq create-lockfiles nil)

  ;; turn on wakatime
  ;; (global-wakatime-mode)

  (if (not (require 'dap-node))
      (progn (dap-node-setup)
             (require 'dap-node)))
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
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(evil-want-Y-yank-to-eol t)
 '(flycheck-checker-error-threshold 2400)
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(org-fontify-done-headline nil)
 '(org-fontify-todo-headline nil)
 '(package-selected-packages
   '(rjsx-mode import-js grizzl add-node-modules-path lv transient polymode anaphora editorconfig yasnippet-snippets org-mime ghub handlebars-mode powerline spinner org-category-capture alert log4e gntp org-plus-contrib markdown-mode json-snatcher json-reformat multiple-cursors hydra parent-mode projectile haml-mode gitignore-mode fringe-helper git-gutter+ git-gutter flyspell-correct pos-tip flycheck pkg-info epl flx magit magit-popup git-commit with-editor smartparens iedit anzu evil goto-chg undo-tree highlight skewer-mode request-deferred websocket request deferred js2-mode simple-httpd diminish autothemer web-completion-data dash-functional tern company bind-map bind-key yasnippet packed anaconda-mode pythonic f dash s helm avy helm-core async auto-complete popup go-guru go-eldoc company-go go-mode swift-mode zenburn-theme yapfify yaml-mode xterm-color ws-butler wolfram-mode winum which-key web-mode web-beautify wakatime-mode volatile-highlights vimrc-mode vi-tilde-fringe uuidgen use-package toc-org thrift tagedit stan-mode sql-indent spaceline smeargle slim-mode shell-pop scss-mode scad-mode sass-mode reveal-in-osx-finder restclient restart-emacs rainbow-mode rainbow-identifiers rainbow-delimiters qml-mode pyvenv pytest pyenv-mode py-isort pug-mode popwin pony-mode pip-requirements persp-mode pcre2el pbcopy paradox osx-trash osx-dictionary orgit org-projectile org-present org-pomodoro org-download org-bullets open-junk-file nginx-mode neotree multi-term move-text mmm-mode matlab-mode markdown-toc magit-gitflow macrostep lua-mode lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode launchctl julia-mode json-mode js2-refactor js-doc jbeans-theme info+ indent-guide hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag gruvbox-theme google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md fuzzy flyspell-correct-helm flycheck-pos-tip flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav ein dumb-jump dockerfile-mode disaster dired+ diff-hl darktooth-theme dactyl-mode cython-mode csv-mode company-web company-tern company-statistics company-c-headers company-anaconda column-enforce-mode color-identifiers-mode coffee-mode cmake-mode clean-aindent-mode clang-format auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile arduino-mode aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell))
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e"))
 '(safe-local-variable-values
   '((eval when
           (require 'rainbow-mode nil t)
           (rainbow-mode 1))))
 '(wakatime-cli-path "/usr/local/bin/wakatime")
 '(wakatime-python-bin nil)
 '(warning-suppress-types '((emacs))))
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(evil-want-Y-yank-to-eol t)
 '(package-selected-packages
   (quote
    (lv transient polymode anaphora editorconfig yasnippet-snippets org-mime ghub handlebars-mode powerline spinner org-category-capture alert log4e gntp org-plus-contrib markdown-mode json-snatcher json-reformat multiple-cursors hydra parent-mode projectile haml-mode gitignore-mode fringe-helper git-gutter+ git-gutter flyspell-correct pos-tip flycheck pkg-info epl flx magit magit-popup git-commit with-editor smartparens iedit anzu evil goto-chg undo-tree highlight skewer-mode request-deferred websocket request deferred js2-mode simple-httpd diminish autothemer web-completion-data dash-functional tern company bind-map bind-key yasnippet packed anaconda-mode pythonic f dash s helm avy helm-core async auto-complete popup go-guru go-eldoc company-go go-mode swift-mode zenburn-theme yapfify yaml-mode xterm-color ws-butler wolfram-mode winum which-key web-mode web-beautify wakatime-mode volatile-highlights vimrc-mode vi-tilde-fringe uuidgen use-package toc-org thrift tagedit stan-mode sql-indent spaceline smeargle slim-mode shell-pop scss-mode scad-mode sass-mode reveal-in-osx-finder restclient restart-emacs rainbow-mode rainbow-identifiers rainbow-delimiters qml-mode pyvenv pytest pyenv-mode py-isort pug-mode popwin pony-mode pip-requirements persp-mode pcre2el pbcopy paradox osx-trash osx-dictionary orgit org-projectile org-present org-pomodoro org-download org-bullets open-junk-file nginx-mode neotree multi-term move-text mmm-mode matlab-mode markdown-toc magit-gitflow macrostep lua-mode lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode launchctl julia-mode json-mode js2-refactor js-doc jbeans-theme info+ indent-guide hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag gruvbox-theme google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md fuzzy flyspell-correct-helm flycheck-pos-tip flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav ein dumb-jump dockerfile-mode disaster dired+ diff-hl darktooth-theme dactyl-mode cython-mode csv-mode company-web company-tern company-statistics company-c-headers company-anaconda column-enforce-mode color-identifiers-mode coffee-mode cmake-mode clean-aindent-mode clang-format auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile arduino-mode aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(safe-local-variable-values
   (quote
    ((eval when
           (require
            (quote rainbow-mode)
            nil t)
           (rainbow-mode 1)))))
 '(wakatime-cli-path "/usr/local/bin/wakatime")
 '(wakatime-python-bin nil))
