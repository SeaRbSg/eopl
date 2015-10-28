; 2.27 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ((lambda (a) (a b)) c)
;
;                                 app-exp
;                               /         \
;                              /           \
;                             /             \
;                         rator             rand
;                           |                 |
;                     lambda-exp              |
;                    /          \          var-exp
;                   /            \            |
;                  /              \           c
;             bound var         app-exp
;                 |              /    \
;                 a             /      \
;                            rator     rand
;                              |         |
;                              |         |
;                           var-exp   var-exp
;                              |         |
;                              a         b

; (lambda (x)
;    (lambda (y)
;      ((lambda (x)
;         (x y))
; x)))
;
;                                 lambda-exp
;                                 /         \
;                                /           \
;                               /             \
;                         bound var           lambda-exp
;                             |                /        \
;                             x               /          \
;                                            /            \
;                                      bound var          app-exp
;                                          |             /       \
;                                          y            /         \
;                                                    rator       rand
;                                                     /             \
;                                                lambda-exp        var-exp
;                                                /        \           |
;                                               /          \          x
;                                              /            \
;                                        bound var        app-exp
;                                            |            /     \
;                                            x           /       \
;                                                      rator    rand
;                                                        |        |
;                                                     var-exp  var-exp
;                                                        |         |
;                                                        x         y


