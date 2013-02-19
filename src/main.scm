;;; Copyright (c) 2012 by Álvaro Castro Castilla
;;; Test for Cairo with OpenGL
(define-structure shot posx posy)
(define-structure enemy posx posy)
(define-structure world gamestate position positionstate bullets enemies shipstate)

(define update-bullet 
  (lambda (shot)
    (shot-posy-set! shot (- (shot-posy shot) 10))))

(define (filter pred lis) ; Sleazing with EQ? makes this
  (let recur ((lis lis))  
    (if (null? lis) lis   ; Use NOT-PAIR? to handle dotted lists.
        (let ((head (car lis))
              (tail (cdr lis)))
          (if (pred head)
              (let ((new-tail (recur tail))) ; Replicate the RECUR call so
                (if (eq? tail new-tail) lis
                    (cons head new-tail)))
              (recur tail))))))

(define (remove pred lis) (filter (lambda (x) (not (pred x))) lis))

(define (main)
  ((fusion:create-simple-gl-cairo '(width: 1280 height: 752))
   (lambda (event world)
     (println (string-append "event: " (object->string event) " ; world: " (object->string world)))
     (let ((type (SDL_Event-type event)))
       (cond
        ((= type SDL_QUIT)
         'exit)
        ((= type SDL_MOUSEBUTTONDOWN)
         (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION "Button down")
         world)
        ((= type SDL_KEYDOWN)
         (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION "Key down")
         (let* ((kevt (SDL_Event-key event))
                (key (SDL_Keysym-sym
                      (SDL_KeyboardEvent-keysym kevt))))
           (cond ((= key SDLK_ESCAPE)
                  'exit)
                 ((= key SDLK_RETURN)
                  (if (eq? (world-gamestate world) 'splashscreen)
                      (make-world 'gamescreen (world-position world) 'idle (world-bullets world) (world-enemies world) (world-shipstate world))
                      (make-world 'splashscreen (world-position world) 'idle (world-bullets world) (world-enemies world) (world-shipstate world))))
                 ((= key SDLK_h)
                  (if (eq? (world-gamestate world) 'splashscreen)
                      (make-world 'highscores (world-position world) 'idle (world-bullets world) (world-enemies world) (world-shipstate world))
                      world))
                 ((= key SDLK_SPACE)
                  (if (not (eq? (world-shipstate world) 'shooting))
                      (if (eq? (world-gamestate world) 'gamescreen)
                          (make-world (world-gamestate world) (world-position world) (world-positionstate world)  (cons (make-shot (+ (world-position world) 20) 650.0) (world-bullets world)) (world-enemies world) 'shooting)
                          world)
                      world)) 
                 ((= key SDLK_LEFT)
                  (if (eq? (world-gamestate world) 'gamescreen)
                      (make-world 'gamescreen (world-position world) 'left (world-bullets world) (world-enemies world) (world-shipstate world))
                      world))
                 ((= key SDLK_RIGHT)
                  (if (eq? (world-gamestate world) 'gamescreen)
                      (make-world 'gamescreen (world-position world) 'right (world-bullets world) (world-enemies world) (world-shipstate world))
                      world))
                 (else
                  (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION (string-append "Key: " (number->string key)))
                  world))))
        ((= type SDL_KEYUP)
         (let* ((kevt (SDL_Event-key event))
                (key (SDL_Keysym-sym
                      (SDL_KeyboardEvent-keysym kevt))))
           (cond ((= key SDLK_LEFT)
                  (if (eq? (world-positionstate world) 'left)
                      (make-world (world-gamestate world) (world-position world) 'idle (world-bullets world) (world-enemies world) (world-shipstate world))
                      world))
                 ((= key SDLK_RIGHT)
                  (if (eq? (world-positionstate world) 'right)
                      (make-world (world-gamestate world) (world-position world) 'idle (world-bullets world) (world-enemies world) (world-shipstate world))
                      world))
                 ((= key SDLK_SPACE)
                  (make-world (world-gamestate world) (world-position world) (world-positionstate world) (world-bullets world) (world-enemies world) 'idle))
                 (else
                  world))))
        (else
         world))))
   (let ((spawntimer 5000) (spawncount 0))
     (lambda (cr time world)
       (println (string-append "time: " (object->string time) " ; world: " (object->string world)))
       ;;(SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION (object->string (SDL_GL_Extension_Supported "GL_EXT_texture_format_BGRA8888")))
       (case (world-gamestate world)
        
         ((splashscreen)
          (cairo_set_source_rgba cr 0.0 0.0 0.0 1.0)
          (cairo_rectangle cr 0.0 0.0 1280.0 752.0)
          (cairo_fill cr)
          (cairo_select_font_face cr "Sans" CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_BOLD)
          (cairo_set_source_rgba cr 1.0 1.0 1.0 0.8)
          (cairo_set_font_size cr 80.0)
          (cairo_move_to cr 275.0 350.0)
          (cairo_show_text cr "SPLASHSCREEN")
          (cairo_fill cr))
         ((gamescreen)
          ;;Update phase
          (if (eq? (world-positionstate world) 'left)
              (if (> (world-position world) 0.0)
                  (world-position-set! world (- (world-position world) 6))))
          (if (eq? (world-positionstate world) 'right)
              (if (< (world-position world) 1240.0)
                  (world-position-set! world (+ (world-position world) 6))))
          (map update-bullet (world-bullets world))



          
          ;;Draw phase
          (cairo_set_source_rgba cr 0.0 0.0 0.0 1.0)
          (cairo_rectangle cr 0.0 0.0 1280.0 752.0)
          (cairo_fill cr)
          (cairo_select_font_face cr "Sans" CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_BOLD)
          (cairo_set_source_rgba cr 1.0 1.0 1.0 0.8)
          (cairo_set_font_size cr 80.0)
          (cairo_move_to cr 300.0 350.0)
          (cairo_show_text cr "GAMESCREEN")
          (cairo_fill cr)
          (let loop ((rest (world-bullets world)))
            (if (null? rest)
                '()
                (begin (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
                       (cairo_rectangle cr (shot-posx (car rest)) (shot-posy (car rest)) 5. 10.0)
                       (cairo_fill cr)
                       (loop (cdr rest)))))
          (world-bullets-set! world (remove (lambda (shot) (< (shot-posy shot) 0)) (world-bullets world)))
          (cairo_set_source_rgba cr 1.0 1.0 1.0 0.8)
          (cairo_rectangle cr (world-position world) 650.0 40.0 40.0)
          (cairo_fill cr))



         ((highscores)
          (cairo_set_source_rgba cr 0.0 0.0 0.0 1.0)
          (cairo_rectangle cr 0.0 0.0 1280.0 752.0)
          (cairo_fill cr)
          (cairo_select_font_face cr "Sans" CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_BOLD)
          (cairo_set_source_rgba cr 1.0 1.0 1.0 0.8)
          (cairo_set_font_size cr 80.0)
          (cairo_move_to cr 300.0 350.0)
          (cairo_show_text cr "HIGHSCORES")
          (cairo_fill cr)))
       world))
   (make-world 
    'splashscreen
    600.0
    'idle
    '()
    '()
    'idle)))
