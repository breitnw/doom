;;; nitree.el --- Nerd icons theme for Treemacs   -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Nick Breitling

;; Author: Nick Breitling <breitling.nw@gmail.com>
;; Keywords: terminals multimedia
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))

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

;; Nerd icons theme for Treemacs, patched for lsp-treemacs support and
;; normalized icon width.

;;; Code:
(require 'nerd-icons)
(require 'treemacs)

(defface nitree-root-face
  '((t (:inherit nerd-icons-cyan)))
  "Face used for the root icon in nerd-icons theme."
  :group 'treemacs-faces)

(defface nitree-file-face
  '((t (:inherit nerd-icons-lcyan)))
  "Face used for the directory and file icons in nerd-icons theme."
  :group 'treemacs-faces)

;; i think this is needed for variable-pitch icons
(defconst nitree-tab (propertize "\t" 'face 'nitree-file-face))

(defun nitree--icon (name set face)
  "Gets the nitree representation of the icon NAME from SET with face FACE.

  SET must be one of \='md, \='fa, \='dev, \='w, \='fl, \='cod, \='ips, \='oct,
  \='pom, \='suc"
  (let ((insert-function
         (cond ((eq set 'md)  #'nerd-icons-mdicon)
               ((eq set 'fa)  #'nerd-icons-faicon)
               ((eq set 'dev) #'nerd-icons-devicon)
               ((eq set 'w)   #'nerd-icons-wicon)
               ((eq set 'fl)  #'nerd-icons-flicon)
               ((eq set 'cod) #'nerd-icons-codicon)
               ((eq set 'ips) #'nerd-icons-ipsicon)
               ((eq set 'oct) #'nerd-icons-octicon)
               ((eq set 'pom) #'nerd-icons-pomicon)
               ((eq set 'suc) #'nerd-icons-sucicon))))
    (nitree--format-icon (funcall insert-function name :face face))))

(defun nitree--format-icon (icon)
  "Formats ICON with a tab afterward to correct spacing."
  (format "%s%s" icon nitree-tab))

(defconst nitree-misc-icons-alist
  '(;; directory icons
    (root-closed  :icon "repo"        :style "oct" :face nitree-root-face)
    (root-open    :icon "repo"        :style "oct" :face nitree-root-face)
    (dir-open     :icon "folder_open" :style "fa" :face nitree-file-face)
    (src-open     :icon "folder_open" :style "fa" :face nitree-file-face)
    (build-open   :icon "folder_open" :style "fa" :face nitree-file-face)
    (test-open    :icon "folder_open" :style "fa" :face nitree-file-face)
    (tag-open     :icon "folder_open" :style "fa" :face nitree-file-face)
    (dir-closed   :icon "folder"      :style "fa" :face nitree-file-face)
    (src-closed   :icon "folder"      :style "fa" :face nitree-file-face)
    (build-closed :icon "folder"      :style "fa" :face nitree-file-face)
    (test-closed  :icon "folder"      :style "fa" :face nitree-file-face)
    (tag-closed   :icon "folder"      :style "fa" :face nitree-file-face)

    ;; misc icons
    (tag-leaf   :icon "tag"           :style "fa" :face nitree-file-face)
    (error      :icon "times_circle"  :style "fa" :face nerd-icons-red)
    (warning    :icon "warning"       :style "fa" :face nerd-icons-yellow)
    (info       :icon "info_circle"   :style "fa" :face nerd-icons-blue)
    (mail       :icon "email_outline" :style "md" :face nitree-file-face)
    (mail-plus  :icon "email_outline" :style "md" :face nitree-file-face)
    (inbox      :icon "email_outline" :style "md" :face nitree-file-face)
    (bookmark   :icon "bookmark"      :style "oct" :face nitree-file-face)
    (screen     :icon "desktop"       :style "fa" :face nitree-file-face)
    (house      :icon "home"          :style "fa" :face nitree-file-face)
    (list       :icon "list"          :style "fa" :face nitree-file-face)
    (repeat     :icon "robot"         :style "md" :face nitree-file-face)
    (suitcase   :icon "archive"       :style "fa" :face nitree-file-face)
    (briefcase  :icon "archive"       :style "fa" :face nitree-file-face)
    (close      :icon "close"         :style "fa" :face nitree-file-face)
    (calendar   :icon "calendar"      :style "fa" :face nitree-file-face)
    (fallback   :icon "file_o"        :style "fa" :face nitree-file-face)))

;; TODO set nerd-icons-corfu-mapping based on this
(defconst nitree-lsp-icons-alist
  '((array          :icon "code_brackets"     :style "md" :face font-lock-type-face)
    (boolean        :icon "cog"               :style "fa" :face font-lock-builtin-face)
    (class          :icon "contao"            :style "dev" :face font-lock-type-face)
    (color          :icon "dashboard"         :style "fa" :face success)
    (command        :icon "terminal"          :style "fa" :face default)
    (constant       :icon "text_box_multiple" :style "md" :face font-lock-constant-face)
    (constructor    :icon "plus_box_multiple" :style "md" :face font-lock-function-name-face)
    (enummember     :icon "tag"               :style "fa" :face font-lock-builtin-face)
    (enum-member    :icon "tag"               :style "fa" :face font-lock-builtin-face)
    (enum           :icon "table_list"        :style "fa" :face font-lock-builtin-face)
    (event          :icon "bolt"              :style "fa" :face font-lock-warning-face)
    (field          :icon "tag"               :style "fa" :face font-lock-variable-name-face)
    (file           :icon "file"              :style "fa" :face font-lock-string-face)
    (folder         :icon "folder"            :style "fa" :face font-lock-doc-face)
    (interface      :icon "server_network"    :style "md" :face font-lock-type-face)
    (key            :icon "key"               :style "fa" :face font-lock-keyword-face)
    (keyword        :icon "key"               :style "fa" :face font-lock-keyword-face)
    (macro          :icon "hashtag"           :style "fa" :face font-lock-keyword-face)
    (magic          :icon "wand"              :style "cod" :face font-lock-builtin-face) ;; TODO
    (method         :icon "function"          :style "md" :face font-lock-function-name-face)
    (function       :icon "function"          :style "md" :face font-lock-function-name-face)
    (module         :icon "archive"           :style "fa" :face font-lock-preprocessor-face)
    (numeric        :icon "pi"                :style "md" :face font-lock-builtin-face)
    (operator       :icon "plus"              :style "fa" :face font-lock-comment-delimiter-face)
    (param          :icon "cogs"              :style "fa" :face default)
    (parameter      :icon "cogs"              :style "fa" :face default)
    (property       :icon "table_list"        :style "fa" :face font-lock-variable-name-face)
    (reference      :icon "file_symlink_directory" :style "oct" :face font-lock-variable-name-face)
    (snippet        :icon "scissors"          :style "fa" :face font-lock-string-face)
    (string         :icon "font"              :style "fa" :face font-lock-string-face)
    (struct         :icon "database"          :style "fa" :face font-lock-variable-name-face)
    (structure      :icon "database"          :style "fa" :face font-lock-variable-name-face)
    (text           :icon "font"              :style "fa" :face font-lock-doc-face)
    (typeparameter  :icon "table_list"        :style "fa" :face font-lock-type-face)
    (type-parameter :icon "table_list"        :style "fa" :face font-lock-type-face)
    (unit           :icon "bar_chart"         :style "fa" :face font-lock-constant-face)
    (value          :icon "pi"                :style "md" :face font-lock-builtin-face)
    (variable       :icon "cog"               :style "fa" :face font-lock-variable-name-face)
    (namespace      :icon "code_braces"       :style "md" :face font-lock-builtin-face)
    (ruler          :icon "gauge"             :style "md" :face font-lock-builtin-face)
    (misc           :icon "code"              :style "fa" :face font-lock-warning-face)
    (t              :icon "code"              :style "fa" :face font-lock-warning-face)

    ;; Treemacs specific
    (boolean-data   :icon "cog"                  :style "fa" :face font-lock-builtin-face)
    (enumerator     :icon "numeric"              :style "md" :face font-lock-builtin-face)
    (indexer        :icon "numeric"              :style "md" :face font-lock-builtin-face)
    (enumitem       :icon "tag"                  :style "fa" :face font-lock-builtin-face)
    (localvariable  :icon "cog"                  :style "fa" :face font-lock-variable-name-face)
    (template       :icon "file_document"        :style "md" :face font-lock-builtin-face)
    (collapsed      :icon "chevron_down"         :style "oct" :face font-lock-comment-face)
    (expanded       :icon "arrow_expand_all"     :style "md" :face font-lock-comment-face)
    (classfile      :icon "cube_outline"         :style "md" :face font-lock-type-face)
    ("class"        :icon "cube_outline"         :style "md" :face font-lock-type-face)
    (default-root-folder-opened :icon "book"     :style "fa" :face nitree-file-face)
    (default-root-folder :icon "book"            :style "fa" :face nitree-file-face)
    (default-folder-opened :icon "folder_open"   :style "fa" :face nitree-file-face)
    (default-folder :icon "folder"               :style "fa" :face nitree-file-face)
    (folder-open    :icon "folder_open"          :style "fa" :face nitree-file-face)
    (folder         :icon "folder"               :style "fa" :face nitree-file-face)
    (folder-type-package-opened :icon "folder_o" :style "fa" :face nitree-file-face)
    (folder-type-package  :icon "folder_o"       :style "fa" :face nitree-file-face)
    (file-type-jar  :icon "java"                 :style "dev" :face font-lock-type-face)
    (jar            :icon "java"                 :style "dev" :face font-lock-type-face)
    (icon-create    :icon "plus"                 :style "fa" :face font-lock-builtin-face)
    (icon-flat      :icon "list"                 :style "fa" :face font-lock-builtin-face)
    (icon-hierarchical :icon "file_tree"         :style "md" :face font-lock-builtin-face)
    (icon-link      :icon "link"                 :style "md" :face font-lock-builtin-face)
    (icon-unlink    :icon "link_off"             :style "md" :face font-lock-builtin-face)
    (library        :icon "book"                 :style "fa" :face font-lock-builtin-face)
    (packagefolder  :icon "folder_o"             :style "fa" :face font-lock-builtin-face)
    (packagefolder-open :icon "folder_open_o"    :style "fa" :face font-lock-builtin-face)
    (packagefolder-open :icon "folder_open_o"    :style "fa" :face font-lock-builtin-face)
    (package        :icon "dropbox"              :style "fa" :face font-lock-builtin-face)
    (java-project   :icon "book_open"            :style "fa" :face font-lock-builtin-face))
  "An association list of LSP feature icons for nitree.

  Also compatible with `nerd-icons-corfu-mapping'.")

(defun nitree--get-icon (icon style face)
  "Return the icon glyph with name ICON, style (fa, md, etc.) STYLE, and face FACE.

  The mapping of kind -> icon is defined by the user in
  `nerd-icons-corfu-mapping'."
  (let* ((icon-fun (intern (concat "nerd-icons-" style "icon")))
         (icon-name (concat "nf-" style "-" icon)))
    (or (and (fboundp icon-fun) (funcall icon-fun icon-name :face face)) "?")))

(treemacs-create-theme "nitree"
  :config
  (progn
    ;; Use the nerd-icons extension alist for regular files
    (dolist (item nerd-icons-extension-icon-alist)
      (let* ((extension (-first-item item))
             (name->icon (-second-item item))
             (name (-third-item item))
             (face (-fifth-item item))
             (icon (nitree--format-icon (funcall name->icon name
                                                 :v-adjust -0.05
                                                 :height 1.0
                                                 :face face)))
             (gui-icons (treemacs-theme->gui-icons treemacs--current-theme))
             (tui-icons (treemacs-theme->tui-icons treemacs--current-theme)))
        (ht-set! gui-icons extension icon)
        (ht-set! tui-icons extension icon)))

    ;; For everything else, use the nitree icon alist
    (dolist (item (append nitree-misc-icons-alist nitree-lsp-icons-alist))
      (let* ((extension (car item))
             (plist (cdr item))
             (icon-name (plist-get plist :icon))
             (style (plist-get plist :style))
             (face (plist-get plist :face))
             (icon (nitree--format-icon (nitree--get-icon icon-name style face)))
             (gui-icons (treemacs-theme->gui-icons treemacs--current-theme))
             (tui-icons (treemacs-theme->tui-icons treemacs--current-theme)))
        (ht-set! gui-icons extension icon)
        (ht-set! tui-icons extension icon)))))

(provide 'nitree)

;;; nitree.el ends here
