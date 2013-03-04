;;; Copyright (c) 2012 by Álvaro Castro Castilla
;;; Test for Cairo with OpenGL
(define-structure star posx posy)
(define-structure shot posx posy)
(define-structure enemy posx posy)
(define-structure world gamestate position positionstate bullets enemies shipstate score highscore stars)

(define game-contents
  '((1 . ((color . (0.0 0.25 0.0))
          (spawntimer . 1000)
          (spawner . 1)))
    (2 . ((color . (0.25 0.0 0.0))
          (spawntimer . 800)
          (spawner . 1)))
    (3 . ((color . (0.0 0.0 0.25))
          (spawntimer . 600)
          (spawner . 1)))
    (4 . ((color . (0.25 0.25 0.0))
          (spawntimer . 400)
          (spawner . 1)))
    (5 . ((color . (0.0 0.25 0.25))
          (spawntimer . 200)
          (spawner . 1)))
    (6 . ((color . (0.25 0.5 0.25))
          (spawntimer . 200)
          (spawner . 2)))
    (7 . ((color . (0.25 0.25 0.5))
          (spawntimer . 200)
          (spawner . 3)))
    (8 . ((color . (0.5 0.25 0.25))
          (spawntimer . 200)
          (spawner . 4)))
    (9 . ((color . (0.5 0.25 0.5))
          (spawntimer . 200)
          (spawner . 5)))
    (10 . ((color . (0.5 0.5 0.5))
          (spawntimer . 200)
          (spawner . 6)))))

(define update-bullet 
  (lambda (shot)
    (shot-posy-set! shot (- (shot-posy shot) 10))))
(define update-enemy
  (lambda (enemy)
    (enemy-posy-set! enemy (+ (enemy-posy enemy) 5))))

(define update-highscore
  (lambda (world)
    (if (> (world-score world) (world-highscore world))
        (world-highscore-set! world (world-score world)))))

(define (create-stars nlist number)
  (let loop ((rest nlist) (counter 0))
    (if (eq? counter number)
        '()
        (cons ( make-star ( exact->inexact (+ (random-integer 660) 315)) (exact->inexact (random-integer 752))) (loop rest (+ counter 1))))))

(define (collision bulletlist enemy world)
  (let loop ((rest bulletlist))
    (unless (null? rest)
            (if (and (or (> (shot-posx (car rest)) (enemy-posx enemy)) (> (+ (shot-posx (car rest)) 5) (enemy-posx enemy)))
                     (< (shot-posx (car rest)) (+ (enemy-posx enemy) 20))
                     (> (- (shot-posy (car rest)) 20) (-(enemy-posy enemy) 20))
                     (< (- (shot-posy (car rest)) 20) (enemy-posy enemy)))
                (begin (shot-posy-set! (car rest) -10.0)
                       (world-score-set! world (+ (world-score world) 10))
                       #t)
            (loop (cdr rest))))))

(define (player-collision enemylist world)
  (let loop ((rest enemylist))
    (unless (null? rest)
            (if (and (or (> (enemy-posx (car rest)) (world-position world)) (> (+ (enemy-posx (car rest)) 20) (world-position world)))
                     (< (enemy-posx (car rest))  (+ (world-position world) 40))
                     (> (enemy-posy (car rest)) 630.0)
                     (< (enemy-posy (car rest)) 650.0))
                #t
                (loop (cdr rest))))))

(define (bullet-collision bulletlist enemylist world)
  (let loop ((rest enemylist))
    (if (null? rest)
        '()
        (if (collision bulletlist (car rest) world)
            (loop (cdr rest))
            (cons (car rest) (loop (cdr rest)))))))

(define (main)
  ((fusion:create-simple-gl-cairo '(width: 1280 height: 752))
   (lambda (event world)
     ;;(println (string-append "event: " (object->string event) " ; world: " (object->string world)))
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
                      (make-world 'gamescreen 600.0 'idle '() '() 'idle 0 (world-highscore world) (create-stars (world-stars world) 200) )
                      (make-world 'splashscreen (world-position world) 'idle (world-bullets world) (world-enemies world) (world-shipstate world) (world-score world) (world-highscore world) (world-stars world))))
                 ((= key SDLK_h)
                  (if (eq? (world-gamestate world) 'splashscreen)
                      (make-world 'highscores (world-position world) 'idle (world-bullets world) (world-enemies world) (world-shipstate world) (world-score world) (world-highscore world) (world-stars world))
                      world))
                 ((= key SDLK_SPACE)
                  (if (not (eq? (world-shipstate world) 'shooting))
                      (if (eq? (world-gamestate world) 'gamescreen)
                          (make-world (world-gamestate world) (world-position world) (world-positionstate world)  (cons (make-shot (+ (world-position world) 17.5) 650.0) (world-bullets world)) (world-enemies world) 'shooting (world-score world) (world-highscore world) (world-stars world))
                          world)
                      world)) 
                 ((= key SDLK_LEFT)
                  (if (eq? (world-gamestate world) 'gamescreen)
                      (make-world 'gamescreen (world-position world) 'left (world-bullets world) (world-enemies world) (world-shipstate world) (world-score world) (world-highscore world) (world-stars world))
                      world))
                 ((= key SDLK_RIGHT)
                  (if (eq? (world-gamestate world) 'gamescreen)
                      (make-world 'gamescreen (world-position world) 'right (world-bullets world) (world-enemies world) (world-shipstate world) (world-score world) (world-highscore world) (world-stars world))
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
                      (make-world (world-gamestate world) (world-position world) 'idle (world-bullets world) (world-enemies world) (world-shipstate world) (world-score world) (world-highscore world) (world-stars world))
                      world))
                 ((= key SDLK_RIGHT)
                  (if (eq? (world-positionstate world) 'right)
                      (make-world (world-gamestate world) (world-position world) 'idle (world-bullets world) (world-enemies world) (world-shipstate world) (world-score world) (world-highscore world) (world-stars world))
                      world))
                 ((= key SDLK_SPACE)
                  (make-world (world-gamestate world) (world-position world) (world-positionstate world) (world-bullets world) (world-enemies world) 'idle (world-score world) (world-highscore world) (world-stars world)))
                 (else
                  world))))
        (else
         world))))
   
   (let ((spawntimer 1000) (spawncount 0) (spawner 1)) 
     (lambda (cr time world)
       (let* ((level-number (floor (+ (/ (world-score world) 100) 1))) (level-contents (cdr (assq level-number game-contents))))
         (if (> level-number 10)
             (set! level-number 10))
         ;;(println (string-append "time: " (object->string time) " ; world: " (object->string world)))
         ;;(SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION (object->string (SDL_GL_Extension_Supported "GL_EXT_texture_format_BGRA8888")))
         (case (world-gamestate world)
           
           ((splashscreen)
            (cairo_set_source_rgba cr 0.0 0.0 0.0 1.0)
            (cairo_rectangle cr 0.0 0.0 1280.0 752.0)
            (cairo_fill cr)
            (cairo_select_font_face cr "Sans" CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_BOLD)
            (cairo_set_source_rgba cr 1.0 1.0 0.0 1.0)
            (cairo_set_font_size cr 80.0)
            (cairo_move_to cr 450.0 350.0)
            (cairo_show_text cr "STARLIN")
            (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
            (cairo_set_font_size cr 40.0)
            (cairo_move_to cr 400.0 450.0)
            (cairo_show_text cr "Space is for everyone")
            (cairo_fill cr))
           
           ((gamescreen)
            ;;Update phase
            (if (eq? (world-positionstate world) 'left)
                (if (> (world-position world) 320.0)
                    (world-position-set! world (- (world-position world) 6))))
            (if (eq? (world-positionstate world) 'right)
                (if (< (world-position world) 920.0)
                    (world-position-set! world (+ (world-position world) 6))))
            (if (> (- time spawncount) spawntimer)
                (begin (set! spawncount time)
                       (let loop ((spawns spawner) (counter 0))
                         (if (eq? spawner counter)
                             '()
                             (begin (world-enemies-set! world (cons (make-enemy (exact->inexact (+ (random-integer 620) 320)) 5.0) (world-enemies world)))
                                    (loop spawns (+ counter 1)))))))
            (update-highscore world)
            (map update-bullet (world-bullets world))
            (map update-enemy (world-enemies world))
            (set! spawntimer (cdr (assq 'spawntimer level-contents)))
            (set! spawner (cdr (assq 'spawner level-contents)))
            
            ;;Draw phase
            (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
            (cairo_rectangle cr 0.0 0.0 1280.0 752.0)
            (cairo_fill cr)
            (cairo_set_source_rgba cr (car (cdr (assq 'color level-contents))) (car (cddr (assq 'color level-contents))) (car (cdddr (assq 'color level-contents))) 1.0)
            (cairo_rectangle cr 10.0 10.0 295.0 732.0)
            (cairo_rectangle cr 975.0 10.0 295.0 732.0)
            (cairo_fill cr)
            (cairo_set_source_rgba cr 0.0 0.0 0.0 1.0)
            (cairo_rectangle cr 315.0 10.0 650.0 732.0)
            (cairo_fill cr)
            (cairo_select_font_face cr "Sans" CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_BOLD)
            (cairo_set_source_rgba cr 0.0 1.0 0.0 0.8)
            (cairo_set_font_size cr 30.0)
            (cairo_move_to cr 990.0 50.0)
            (cairo_show_text cr "Score: ")
            (cairo_set_source_rgba cr 1.0 1.0 1.0 0.8)
            (cairo_set_font_size cr 30.0)
            (cairo_move_to cr 1200.0 50.0)
            (cairo_show_text cr (number->string (world-score world)))
            (cairo_set_source_rgba cr 0.0 1.0 0.0 0.8)
            (cairo_set_font_size cr 30.0)
            (cairo_move_to cr 990.0 80.0)
            (cairo_show_text cr "HighScore: ")
            (cairo_set_source_rgba cr 1.0 1.0 1.0 0.8)
            (cairo_set_font_size cr 30.0)
            (cairo_move_to cr 1200.0 80.0)
            (cairo_show_text cr (number->string (world-highscore world)))
            (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
            (cairo_set_font_size cr 60.0)
            (cairo_move_to cr 20.0 300.0)
            (cairo_show_text cr "LEVEL")
            (cairo_move_to cr 250.0 300.0)
            (cairo_show_text cr  (number->string level-number))

            
            (let loop ((rest (world-stars world)))
              (if (null? rest)
                  '()
                  (begin (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
                         (cairo_rectangle cr (star-posx (car rest)) (star-posy (car rest)) 1.0 1.0)
                         (cairo_fill cr)
                         (loop (cdr rest)))))
            (let loop ((rest (world-bullets world)))
              (if (null? rest)
                  '()
                  (begin (cairo_set_source_rgba cr 1.0 1.0 0.0 1.0)
                         (cairo_rectangle cr (shot-posx (car rest)) (shot-posy (car rest)) 5.0 10.0)
                         (cairo_fill cr)
                         (loop (cdr rest)))))
            (let loop ((rest (world-enemies world)))
              (if (null? rest)
                  '()
                  (begin (cairo_set_source_rgba cr 0.0 1.0 1.0 1.0)
                         (cairo_rectangle cr (enemy-posx (car rest)) (enemy-posy (car rest)) 20.0 20.0)
                         (cairo_fill cr)
                         (loop (cdr rest)))))
            (world-bullets-set! world (remove (lambda (shot) (< (shot-posy shot) 0)) (world-bullets world)))
            (world-enemies-set! world (remove (lambda (enemy) (> (enemy-posy enemy) 752)) (world-enemies world)))
            (world-enemies-set! world (bullet-collision (world-bullets world) (world-enemies world) world))
            (if (player-collision (world-enemies world) world)
                (world-gamestate-set! world 'lost))
            (cairo_set_source_rgba cr 1.0 0.0 0.0 0.8)
            (cairo_rectangle cr (world-position world) 650.0 40.0 40.0)
            (cairo_fill cr))
           
           ((lost)
            (cairo_set_source_rgba cr 0.25 0.0 0.0 .5)
            (cairo_rectangle cr 0.0 0.0 1280.0 752.0)
            (cairo_fill cr)
            (cairo_select_font_face cr "Sans" CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_BOLD)
            (cairo_set_source_rgba cr 1.0 1.0 0.0 0.8)
            (cairo_set_font_size cr 80.0)
            (cairo_move_to cr 350.0 350.0)
            (cairo_show_text cr "YOU LOST")
            (cairo_set_source_rgba cr 1.0 1.0 1.0 0.8)
            (cairo_move_to cr 350.0 500.0)
            (cairo_show_text cr "SCORE:")
            (cairo_move_to cr 760.0 500.0)
            (cairo_show_text cr (number->string (world-highscore world)))
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
         world)))
   (make-world 
    'splashscreen
    600.0
    'idle
    '()
    '()
    'idle
    0
    0
    '())))
