#lang racket/gui
(require racket/list)
(require racket/draw)
;; Define stations and their coordinates
(define stations
  '((1 "Borough" 100 100)
    (2 "Tower Hill" 200 100)
    (3 "Angel" 300 100)
    (4 "Moorgate" 100 200)
    (5 "Limehouse" 200 200)
    (6 "Canary Wharf" 300 200)
    (7 "Abbey Wood" 100 300)
    (8 "Euston" 200 300)
    (9 "Bank" 300 300)
    (10 "Waterloo" 200 400)))
;; Define connections between stations
(define connections
  '((1 2 4)
    (2 1 3 5 6)
    (3 2 7)
    (4 1 8)
    (5 2 9)
    (6 2 9)
    (7 3)
    (8 4 10)
    (9 5 6)
    (10 8)))
;; Define costs for traveling between stations
(define costs
  (hash
   1 25.0
   2 30.0
   3 27.5
   4 29.0
   5 32.0
   6 26.0
   7 31.0
   8 28.0
   9 33.0
   10 29.5))
(define station-radius 15) ; Radius for drawing station circles
;; Function to draw the graph
(define (draw-graph dc)
  (send dc set-scale 1 1)
  ;; Draw stations
  (for ((station stations))
    (let* ((id (car station))
           (name (cadr station))
           (x (caddr station))
           (y (cadddr station)))
      (send dc draw-text name x y)
      (send dc draw-ellipse (- x station-radius) (- y station-radius) (* 2 station-radius) (* 1 station-radius))
      ;; Add station ID text
      (send dc draw-text (number->string id) (- x station-radius) (- y station-radius))))
  ;; Draw connections between stations
  (for ((connection connections))
    (let* ((from (car connection))
           (to-list (cdr connection))
           (x1 (caddr (assoc from stations)))
           (y1 (cadddr (assoc from stations))))
      (for ((to to-list))
        (let* ((x2 (caddr (assoc to stations)))
               (y2 (cadddr (assoc to stations))))
          (send dc draw-line x1 y1 x2 y2))))))
;; Define a canvas class for drawing
(define my-canvas%
  (class canvas%
    (super-new)
    (define/override (on-paint)
      (let ((dc (send this get-dc)))
        (draw-graph dc)))
    (define/override (on-event event)
      ;; Handle additional events if needed
      (void))))
;; Create a frame for the GUI
(define frame (new frame%
                  [label "Transportation Network Map"]
                  [width 500]
                  [height 500]))
;; Create a canvas widget inside the frame
(define canvas (new my-canvas%
                   [parent frame]
                   [stretchable-width #t]
                   [stretchable-height #t]
                   [min-height 400]))
(send frame show #t) ; Display the frame
;; Function to find all possible routes between two stations
(define (find-all-routes start end)
  (define (loop current-station visited path)
    (let loop ((current-station current-station)
               (visited visited)
               (path path))
      (define path+ (append path (list current-station)))
      (cond
        ((= current-station end) (list path+))
        ((not (assq current-station connections)) '())
        (else
         (let* ((new-routes
                 (filter-map
                  (lambda (station)
                    (and (not (member station visited))
                         (loop station (cons current-station visited) path+)))
                  (cdr (assq current-station connections)))))
           (apply append new-routes))))))
  (loop start '() '()))
;; Define a variable to store the previous route
(define previous-route '())
;; Function to highlight routes on the map
(define (highlight-routes routes canvas)
  (let ((dc (send canvas get-dc)))
    ;; Erase previous routes
    (send dc set-pen "gray" 2 'solid)
    (for ((i (in-range (sub1 (length previous-route)))))
      (let* ((station1 (list-ref previous-route i))
             (station2 (list-ref previous-route (add1 i)))
             (x1 (caddr (assoc station1 stations)))
             (y1 (cadddr (assoc station1 stations)))
             (x2 (caddr (assoc station2 stations)))
             (y2 (cadddr (assoc station2 stations))))
        (send dc draw-line x1 y1 x2 y2)))
    ;; Highlight current routes
    (for ((route routes)
          (color (list "green" "red")))
      (send dc set-pen color 2 'solid)
      (for ((i (in-range (sub1 (length route)))))
        (let* ((station1 (list-ref route i))
               (station2 (list-ref route (add1 i)))
               (x1 (caddr (assoc station1 stations)))
               (y1 (cadddr (assoc station1 stations)))
               (x2 (caddr (assoc station2 stations)))
               (y2 (cadddr (assoc station2 stations))))
          (send dc draw-line x1 y1 x2 y2))))
    ;; Update the previous route
    (set! previous-route (apply append routes))))
(define station-time 2) ; Time taken to travel between stations
;; Function to calculate total cost and time for a given route
(define (calculate-total-cost route)
  (let loop ((route route) (total-cost 0) (total-time 0))
    (if (null? (cdr route))
        (list (round total-cost) total-time) ; Return total cost and time
        (let* ((station1 (car route))
               (station2 (cadr route))
               (cost-time (calculate-cost station1 station2 costs))
               (cost (car cost-time))
               (time (cadr cost-time)))
          (loop (cdr route) (+ total-cost cost) (+ total-time time))))))
;; Function to calculate cost and time between two stations
(define (calculate-cost from-station to-station costs)
  (let* ((distance (abs (- (hash-ref costs from-station) (hash-ref costs to-station))))
         (segments (length (find-all-routes from-station to-station))))
    (list distance (* station-time segments)))) ; Return cost and time
;; Function to calculate routes between two stations
(define (calculate-routes start end)
  (define (valid-station? station)
    (and (number? station) (<= 1 station 10)))
  (define start-station (string->number (send start-station-var get-value)))
  (define end-station (string->number (send end-station-var get-value)))
  (cond
    ((or (not (valid-station? start-station)) (not (valid-station? end-station)))
     "Invalid station numbers. Please enter valid station numbers (1 to 10).")
    ((empty? (find-all-routes start-station end-station))
     "No route found between the selected stations.")
    (else
     (let ((all-routes (find-all-routes start-station end-station)))
       (highlight-routes all-routes canvas) ; Highlight routes on the map
       (let* ((formatted-routes
               (map (lambda (route)
                      (let ((cost-time (calculate-total-cost route)))
                        (format "Route: ~a, Cost: ~a£, Time: ~a minutes\n" (map (lambda (station-id) (cadr (assoc station-id stations))) route) (car cost-time) (cadr cost-time))))
                    all-routes)))
         (apply string-append formatted-routes))))))
;function to reset UI
(define (reset-ui)
  (send start-station-var set-value "")
  (send end-station-var set-value "")
  (send result-label set-label "Result will be displayed here.")
  (highlight-routes '() canvas)) ; Clear highlighted routes on the map
;; Create GUI elements for input and result display
(define start-label (new message%
                      [parent frame]
                      [label "From Station (Please enter the number(e.g 1)):"]))
(define start-station-var (new text-field%
                            [parent frame]
                            [label #f]
                            [init-value ""]))
(define end-label (new message%
                    [parent frame]
                    [label "To Station (Please enter the number(e.g 10)):"]))
(define end-station-var (new text-field%
                          [parent frame]
                          [label #f]
                          [init-value ""]))
(define (exit-app button event)
  (send frame show #f))
(define calculate-button (new button%
                             [parent frame]
                             [label "Calculate Routes"]
                             [callback (lambda (button event)
                                         (let* ((start-station (string->number (send start-station-var get-value)))
                                                (end-station (string->number (send end-station-var get-value))))
                                           (send result-label set-label (calculate-routes start-station end-station))))]))
(define result-label (new message%
                      [parent frame]
                      [label "Result will be displayed here."]
                      [min-height 100] ; Set the minimum height
                      [min-width 1000] ; Set the minimum width
                      [stretchable-width #t]
                      [stretchable-height #t]
                      [font (make-object font% 10 'default 'normal)])) ; Adjust font size
(define exit-button (new button%
                      [parent frame]
                      [label "Exit"]
                      [callback exit-app]))
(define route-frame (new vertical-panel%
                      [parent frame]
                      [alignment '(center center)]))
(send frame show #t) ; Display the frame