;;; modules/app/calendar.el -*- lexical-binding: t; -*-

;; org and gcal calendar

;; custom ----------------------------------------------------------------------

;; loading calendars and agendas
(defvar my-calendar-base-dir "~/Documents/org/calendar/")
(defvar my-calendar-inbox-subdir "inbox/")
(defvar my-calendar-state-subdir "state/")

(defvar my-calendar-specs
  '((:name "meetings"
     :color yellow
     :id "breitnw/ba5d2c92-5382-8c25-f30f-5267a413f9f9")
    (:name "volunteering"
     :color magenta
     :id "breitnw/e011c882-cbb6-2b85-ae30-b554980a2a58")
    (:name "classes"
     :color green
     :id "breitnw/05c1abae-04ae-7c3c-6424-dbcb66b4e9a4")
    (:name "stuff"
     :color red
     :id "breitnw/b418ade2-7d26-c0c6-3a65-102cec6f0392")
    (:name "clubs"
     :color cyan
     :id "breitnw/3dcdba38-60a3-3005-4243-93fcec8ce978")))

(defvar my-agenda-specs
  '((:name "agenda"
     :color violet
     :path "~/Documents/org/agenda/todos.org")))

;; calendar paths
(defun my--make-calendar-path (cal-spec)
  "make a string representing the path to a calendar inbox, based on CAL-SPEC"
  (concat (file-name-as-directory my-calendar-base-dir)
          (file-name-as-directory my-calendar-inbox-subdir)
          (plist-get cal-spec :name) ".org"))

;; calendar sources (calfw)
(defun my--make-calendar-source (cal-spec)
  "make a calfw-org source based on CAL-SPEC"
  (cfw:org-create-file-source
   (plist-get cal-spec :name)
   (my--make-calendar-path cal-spec)
   (doom-color (plist-get cal-spec :color))))

(defun my--make-calendar-sources ()
  "make a list of calfw-org sources based on variable `my-calendar-specs'"
  (mapcar #'my--make-calendar-source my-calendar-specs))

;; calendar sources (org-caldav)
(defun my--make-caldav-source (cal-spec)
  "make a org-caldav source based on CAL-SPEC"
  (list :calendar-id (plist-get cal-spec :id)
        :files nil
        :inbox (my--make-calendar-path cal-spec)))

(defun my--make-caldav-sources ()
  "make a list of org-caldav sources based on `my-calendar-specs'"
  (mapcar #'my--make-caldav-source my-calendar-specs))

;; agenda sources (calfw)
(defun my--make-agenda-source (ag-spec)
  "make a calfw-org agenda source based on AG-SPEC"
  (cfw:org-create-file-source
   (plist-get ag-spec :name)
   (plist-get ag-spec :path)
   (doom-color (plist-get ag-spec :color))))

(defun my--make-agenda-sources ()
  "make a list of calfw-org agenda sources based on variable `my-agenda-specs'"
  (mapcar #'my--make-agenda-source my-agenda-specs))


;; org-caldav ------------------------------------------------------------------

;; org-caldav syncs back and forth between my caldav server
(use-package! org-caldav
  :defer t
  :custom
  (org-caldav-url "https://cal.mndco11age.xyz")
  (org-icalendar-timezone "America/Chicago")
  (org-caldav-calendars (my--make-caldav-sources))
  (org-caldav-save-directory (concat my-calendar-base-dir
                                     my-calendar-state-subdir)))

;; function to sync the open calendar
(defun my-sync-calendar ()
  (interactive)
  ;; only try to sync if we have the cfw buffer open; provides an easy
  ;; way to prevent sync behavior
  (when (get-buffer "*cfw-calendar*")
    ;; requires org-caldav to fetch
    (doom-require 'org-caldav)
    ;; remember the previous preference to show sync results
    (setq-local sync-tmp org-caldav-show-sync-results
                org-caldav-show-sync-results nil)
    ;; sync the calendar and refresh the buffer
    (org-caldav-sync)
    (cfw:refresh-calendar-buffer t)
    ;; reset the preference to show sync results
    (setq-local org-caldav-show-sync-results sync-tmp)))


;; calfw -----------------------------------------------------------------------

;; function to open my custom calendar, with agendas and events
(defun my-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :view 'block-5-day
   :contents-sources (append (my--make-calendar-sources)
                             (my--make-agenda-sources))))

;; enable orgmode integration
(use-package! calfw-org
  :after calfw)

;; improve layout with calfw-blocks
(use-package! calfw-blocks
  :after calfw
  :config
  ;; customize line faces
  (custom-set-faces!
    `(calfw-blocks-overline
      :overline ,(doom-color 'bg-alt)
      :weight normal))

  ;; customize block faces
  (defun my-dark-color (col)
    (doom-blend (doom-color col) (doom-color 'bg) 0.3))

  (let ((face-alist '((calfw-blocks-face-red . red)
                      (calfw-blocks-face-orange . orange)
                      (calfw-blocks-face-yellow . yellow)
                      (calfw-blocks-face-green . green)
                      (calfw-blocks-face-teal . teal)
                      (calfw-blocks-face-cyan . cyan)
                      (calfw-blocks-face-blue . blue)
                      (calfw-blocks-face-magenta . magenta)
                      (calfw-blocks-face-violet . violet))))
    (dolist (elt face-alist)
      (eval `(defface ,(car elt)
               '((t (:foreground ,(doom-color (cdr elt))
                     :background ,(my-dark-color (cdr elt)))))
               "face for calfw-blocks"
               :group 'calfw-blocks-faces))
      (eval `(defvar ,(car elt) ',(car elt)))))

  (setq ;;calfw-blocks-earliest-visible-time '(8 0)
   calfw-blocks-initial-visible-time '(8 0)
   calfw-blocks-lines-per-hour 2
   calfw-blocks-faces-list (list calfw-blocks-face-red
                                 calfw-blocks-face-orange
                                 calfw-blocks-face-green
                                 calfw-blocks-face-blue
                                 calfw-blocks-face-violet)))

;; calfw provides a pretty calendar to view weeks, months, etc.
(use-package! calfw
  :autoload #'cfw:open-calendar-buffer
  :after doom-themes
  :custom
  ;; fancy character junctions
  (cfw:fchar-junction ?┼)
  (cfw:fchar-vertical-line ?│)
  (cfw:fchar-horizontal-line ?─)
  (cfw:fchar-left-junction ?├)
  (cfw:fchar-right-junction ?┤)
  (cfw:fchar-top-junction ?┬)
  (cfw:fchar-top-left-corner ?╭)
  (cfw:fchar-top-right-corner ?╮)
  ;; wrap lines nicely
  (cfw:render-line-breaker #'cfw:render-line-breaker-wordwrap)

  :config
  ;; fix keymaps for colemak
  ;; TODO somehow integrate this into keymaps.el, or even Nix config?
  (map! :map cfw:calendar-mode-map
        ;; Vi style
        :m "h" #'calfw-blocks-navi-previous-day-command
        :m "n" #'evil-next-visual-line
        :m "e" #'evil-previous-visual-line
        :m "i" #'calfw-blocks-navi-next-day-command
        :m "N" #'big-scroll-down
        :m "E" #'big-scroll-up
        :m "g" #'cfw:navi-goto-date-command
        :m "." #'cfw:navi-goto-today-command
        :m "<backtab>" #'cfw:navi-prev-item-command
        :m "<tab>" #'cfw:navi-next-item-command

        :m "<RET>" #'cfw:show-details-command
        :m "q" #'my-close-calendar
        :m "s" #'my-sync-calendar)

  ;; close details buffer with q
  (map! :map cfw:details-mode-map
        :nmv "q" #'+workspace/close-window-or-workspace)

  ;; beautify faces a bit
  ;; NOTE: this can't be done with :custom-face, since `doom-color' needs to be
  ;; called after the theme is loaded.
  (setq cfw:face-item-separator-color (doom-color 'base3))
  (custom-set-faces!
    ;; calfw faces
    `(cfw:face-title              :foreground ,(doom-color 'blue) :weight bold)
    `(cfw:face-header             :foreground ,(doom-color 'teal) :weight bold)
    `(cfw:face-sunday             :foreground ,(doom-color 'comments) :weight bold)
    `(cfw:face-saturday           :foreground ,(doom-color 'comments) :weight bold)
    `(cfw:face-holiday            :foreground ,(doom-color 'comments))
    `(cfw:face-grid               :foreground ,(doom-color 'base5))
    `(cfw:face-periods            :foreground ,(doom-color 'cyan))
    `(cfw:face-day-title          :foreground ,(doom-color 'comments))
    `(cfw:face-default-day        :foreground ,(doom-color 'comments))
    `(cfw:face-annotation         :foreground ,(doom-color 'magenta))
    `(cfw:face-disable            :foreground ,(doom-color 'comments))
    `(cfw:face-today-title        :foreground ,(doom-color 'magenta) :background ,(doom-color 'base3) :weight bold)
    `(cfw:face-today              :weight bold)
    `(cfw:face-select             :background ,(doom-color 'base4))
    `(cfw:face-toolbar            :foreground ,(doom-color 'comments))
    `(cfw:face-toolbar-button-off :foreground ,(doom-color 'comments) :weight bold)
    `(cfw:face-toolbar-button-on  :foreground ,(doom-color 'teal) :weight bold))

  ;; function to close the open calendar
  (defun my-close-calendar ()
    (interactive)
    (cfw:org-clean-exit)
    ;; if there weren't any buffers to bury beneath, just
    ;; open the dashboard
    (when (equal (buffer-name) "*cfw-calendar*")
      (+doom-dashboard/open (selected-frame))))

  ;; timer for refreshing the scale of calendar after resize
  (defvar current-resize-timer nil)
  (defun resize-display (&rest _)
    (when current-resize-timer
      (cancel-timer current-resize-timer))
    (setq current-resize-timer
          (run-with-idle-timer
           0.1 nil
           (lambda ()
             (when (eq major-mode #'cfw:calendar-mode)
               (cfw:cp-resize (cfw:cp-get-component)
                              (window-width)
                              (window-height)))))))

  ;; when the calendar is opened...
  (add-hook! cfw:calendar-mode
             ;; don't allow editing
             (evil-motion-state)
             ;; automatically redraw on window size change
             (add-to-list 'window-size-change-functions #'resize-display)
             ;; we want to wait for the calendar to actually open before
             ;; operating on it
             (run-with-idle-timer
              1 nil (lambda ()
                      ;; resize the buffer
                      (resize-display)
                      ;; fetch the calendar
                      (my-sync-calendar))))

  ;; when the details popup is opened...
  (add-hook! cfw:details-mode
    (evil-colemak-basics-mode)
    (evil-motion-state)))

