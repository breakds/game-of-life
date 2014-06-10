;;;; game-of-life.asd

(asdf:defsystem #:game-of-life
    :serial t
    :depends-on (#:basicl
                 #:parenscript
                 #:realispic)
    :components ((:file "game")))
