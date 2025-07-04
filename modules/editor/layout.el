;;; modules/editor/layout.el -*- lexical-binding: t; -*-

;; utilities to customize the keyboard layout when using evil-mode

;; enable global mode
(global-evil-colemak-basics-mode)

(defvar layout-priority-keys
  '(?a ?r ?s ?t ?n ?e ?i ?o ?d ?h)
  "The keys that are most accessible to the user, for tools such as Avy.")

(defvar layout-motion-keys
  '(?h ?n ?e ?i)
  "The keys used for motion left, down, up, and right, respectively.")

(defun layout--motion-key (direction)
  "Gets the key corresponding with DIRECTION in the layout."
  (cond ((eq direction 'left)  (car layout-motion-keys))
        ((eq direction 'down)  (cadr layout-motion-keys))
        ((eq direction 'up)    (caddr layout-motion-keys))
        ((eq direction 'right) (cadddr layout-motion-keys))))

(defun layout-remap-keys ()
  "Remap keys for the current layout"

  ;; for convenience, map from chars to strings
  (setq lpk-str (mapcar #'string layout-priority-keys)
        lmk-str (mapcar #'string layout-motion-keys))

  ;; avy: search for characters on the screen
  (after! avy
    (setq! avy-keys layout-priority-keys))

  ;; colemak keybinds for evil-org
  (after! evil-org
    ;; HACK actually unmap conflicting keys properly, and make sure they get
    ;; remapped. this one is (somehow) remapped, but idk where
    (map! :map 'evil-org-mode-map
          :ov "i" nil)

    (setq evil-org-movement-bindings
          `((left  . ,(string (layout--motion-key 'left)))
            (down  . ,(string (layout--motion-key 'down)))
            (up    . ,(string (layout--motion-key 'up)))
            (right . ,(string (layout--motion-key 'right)))))))

(layout-remap-keys)
