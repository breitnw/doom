;; avy: search for characters on the screen
(after! avy
  (setq avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o ?d ?h
                   ?w ?f ?p ?l ?u ?y)))

;; evil-snipe is used solely for one-character motions
(after! evil-snipe
  (setq evil-snipe-scope 'visible))
