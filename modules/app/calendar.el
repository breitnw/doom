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

;; calfw -----------------------------------------------------------------------

;; calfw provides a pretty calendar to view weeks, months, etc.
(use-package! calfw
  :config
  ;; enable orgmode integration
  (require 'calfw-org)

  ;; timer for refreshing the scale of calendar after resize
  (defvar current-refresh-display-timer nil)
  (defun refresh-display (&rest _)
    (when current-refresh-display-timer
      (cancel-timer current-refresh-display-timer))
    (setq current-refresh-display-timer
          (run-with-idle-timer
           1 nil
           (lambda ()
             (when (eq major-mode #'cfw:calendar-mode)
               (message "refreshing calendar size")
               (cfw:refresh-calendar-buffer nil)
               (message "calendar size refreshed!"))))))

  ;; make faces less ugly
  (custom-set-faces!
    `(cfw:face-title :foreground ,(doom-color 'blue) :weight bold)
    `(cfw:face-header :foreground ,(doom-color 'teal) :weight bold)
    `(cfw:face-sunday :foreground ,(doom-color 'comments) :weight bold)
    `(cfw:face-saturday :foreground ,(doom-color 'comments) :weight bold)
    `(cfw:face-holiday :foreground ,(doom-color 'comments) :background ,(doom-color 'bg))
    `(cfw:face-grid :foreground ,(doom-color 'base5))
    `(cfw:face-default-content :foreground ,(doom-color 'fg))
    `(cfw:face-periods :foreground ,(doom-color 'cyan))
    `(cfw:face-day-title :foreground ,(doom-color 'comments))
    `(cfw:face-default-day :weight bold :inherit cfw:face-day-title)
    `(cfw:face-annotation :foreground ,(doom-color 'magenta) :inherit cfw:face-day-title)
    `(cfw:face-disable :inherit cfw:face-day-title)
    `(cfw:face-today-title :foreground ,(doom-color 'magenta) :background ,(doom-color 'base3) :weight bold)
    `(cfw:face-today :background: ,(doom-color 'orange) :weight bold)
    `(cfw:face-select :background ,(doom-color 'base4))
    `(cfw:face-toolbar :foreground ,(doom-color 'fg))
    `(cfw:face-toolbar-button-off :foreground ,(doom-color 'comments) :weight bold)
    `(cfw:face-toolbar-button-on :foreground ,(doom-color 'teal) :weight bold))

  ;; revert to ascii borders
  (setq cfw:fchar-junction ?┼
        cfw:fchar-vertical-line ?│
        cfw:fchar-horizontal-line ?─
        cfw:fchar-left-junction ?├
        cfw:fchar-right-junction ?┤
        cfw:fchar-top-junction ?┬
        cfw:fchar-top-left-corner ?╭
        cfw:fchar-top-right-corner ?╮)

  ;; wrap lines nicely
  (setq cfw:render-line-breaker #'cfw:render-line-breaker-wordwrap
        cfw:face-item-separator-color (doom-color 'base3))

  ;; function to open my custom calendar, with agendas and events
  (defun my-open-calendar ()
    (interactive)
    (cfw:open-calendar-buffer
     :view 'two-weeks
     :contents-sources (append (my--make-calendar-sources)
                               (my--make-agenda-sources))))

  ;; when the calendar is opened...
  (add-hook! cfw:calendar-mode
             ;; don't allow editing
             (evil-motion-state)
             ;; automatically redraw on window size change
             (add-to-list 'window-size-change-functions #'refresh-display)
             ;; refresh the buffer
             (cfw:refresh-calendar-buffer nil))

  ;; when the details popup is opened
  (add-hook! cfw:details-mode
    (evil-colemak-basics-mode)
    (evil-motion-state)))

;; org-caldav ------------------------------------------------------------------

;; org-caldav syncs back and forth between my caldav server
(after! org-caldav
  :config
  (setq org-caldav-url "https://cal.mndco11age.xyz"
        org-icalendar-timezone "America/Chicago"
        org-caldav-calendars (my--make-caldav-sources)
        org-caldav-save-directory (concat my-calendar-base-dir
                                          my-calendar-state-subdir)))
