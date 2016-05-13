#lang scheme



;; include this library to use the Basic hash tables
(require srfi/69)



;; room descriptions association list
(define descriptions '( (1 "You are in the lobby, Available direction is 'North'")
                        (2 "This is where the treasure Gold is, Available direction is 'East'")
                        (3 "You are in the hallway, Available direction is 'North' and 'West'")
                        (4 "You are in a swamp, Available directions are 'North', 'South', and 'East'")
                        (5 "You are in the sitting room, Available directions are 'West', and 'North'")
                        (6 "you are in the toilet, Available directions are 'North', and 'South'")
                        (7 "you are in the kitchen, Available direction is 'South'")
                        (8 "you are in the bathroom, Available directions are 'South', and 'East'")
                        (9 "You are in the corridor, Available directions are 'West', and 'South'")))

;; rooms directions
(define directions '( (1 (north 4) (south 0) (west 0) (east 0))
                      (2 (north 0) (south 0) (west 0) (east 3))
                      (3 (north 6) (south 0) (west 2) (east 0))
                      (4 (north 7) (south 1) (west 0) (east 5))
                      (5 (north 8) (south 0) (west 4) (east 0))
                      (6 (north 9) (south 3) (west 0) (east 0))
                      (7 (north 0) (south 4) (west 0) (east 0))
                      (8 (north 0) (south 5) (west 0) (east 9))
                      (9 (north 0) (south 6) (west 8) (east 0))))

;; rooms objectss
(define objectInRoom '( (1 (closed #f) (key #f) (end #f))
                      (2 (closed #f) (key #f)  (end #t))
                      (3 (closed #f) (key #f)  (end #f))
                      (4 (closed #f) (key #f)  (end #f))
                      (5 (closed #t) (key #f)  (end #f))
                      (6 (closed #t) (key #f)  (end #f))
                      (7 (closed #f) (key #t)  (end #f))
                      (8 (closed #f) (key #f)  (end #f))
                      (9 (closed #f) (key #t)  (end #f))))

; now load objectInRoom in to the rooms hash table(roomsdb)
; make-hash-table is a constructor here
(define roomsdb (make-hash-table))

;; loop through all items in the rooms-list and insert them into the hash table
(for-each (lambda (y) 
            (hash-table-set! roomsdb (car y) (cdr y))) objectInRoom)

; room items descriptions
(define items_descript '((welcome " >>>[ You are Welcome   ]<<<\n >>>[ You need to search for a misplaced Gold to win the game!  ]<<<") 
                
                     (closed "This door is closed, type 'open' to open the door")
                     (key "There is a 'Key' in this room, type 'take' to take it")
                     (no-key "You do not have any key to open this room")
                     
                     (take "You have taken the key in this room")
                     (take-error "There is no key in this room to take")
                     (open "You have used a key and opened the door")
                     (open-error "This door is not closed")
                     (found_it "You Found it, you have found the misplaced Gold\n    You can take it!")))

;; create a new hash table for attributes of the player
(define player (make-hash-table))
;; sets the value associated to 0 in player, and the previous association (if any) is removed
(hash-table-set! player 0 '(1 (keys 0)))

;; update a player attribute by given key and new value
(define (player-update key value)
  (hash-table-set! player 0 (cons '1 (change-room-item  (cdr (hash-table-ref player 0)) key value))))

;; get an attribute of the player by given key
(define (player-attribute key)
  (car (assq-ref (cdr (hash-table-ref player 0)) key)))

;; increase a value of player attribute
(define (player-add-value key value)
  (let ((new-value (+ (player-attribute key) value)))
    (player-update key (list key new-value))))

;; prints player item
(define (player-status)
  (printf "  >>>[  ")
  (for-each (lambda (y) (printf "~a: ~a  " (car y) (cadr y))) (cdr (hash-table-ref player 0)))
  (printf "]<<<\n"))

;; gets list in the asociacion list by id
(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))

;; Get an attribute in the given asociacion list by id
(define (lookup data room-id attribute)
  (car (assq-ref (assq-ref data room-id) attribute)))

;; gets the room description by given id
(define (get-room-description rid)
  (car (assq-ref descriptions rid)))

;; returns a text description of the item in the roomsdb
(define (describe item)
  (printf "~a\n" (car (assq-ref items_descript item))))

;; get available directions
(define (get-available-directions rid)
  (let ((direction (assq-ref directions rid)))
    (map car (filter (lambda (y) (> (second y) 0)) direction))))

;; print available commands and directions in the given room
(define (print-available-commands rid)
  (printf ">>>>>[  ")
  (for-each (lambda (y) (printf "~a  " y)) (append (get-available-directions rid) (get-available-commands rid))) 
  (printf "]<<<<<\n"))

;; checks roomsdb hash table and returns value of an attribute in the roomsdb
(define (get-room-item id field)
  ;; checks whether there is any association to id in roomsdb
  (if (hash-table-exists? roomsdb id)
      (let ((record (hash-table-ref roomsdb id)))
        (if (memq field '(closed key end))
          (cadr (assq field record))
          "wrong field type"))
      "no such room"))

;; checks roomsdb and returns the list with given room id
(define (get-room id)
  ;;  tells whether there is any association to id in roomsdb
  (if (hash-table-exists? roomsdb id)
      (hash-table-ref roomsdb id)
      "no such room"))

;; returns all available items in the given room id
(define (get-available-items rid)
  (let ((direction (get-room rid)))
    (map car (filter (lambda (y) (eq? (second y) #t)) direction))))

;; gets closed doors in the given room
(define (get-closed-room rid)
  (filter 
   (lambda (y) (get-room-item y 'closed)) 
   (map (lambda (y) (lookup directions rid y ))  (get-available-directions rid))))

; returns a list of available commands depending on the roomsdb attributes
(define (get-available-commands rid)
  (let ((t (map map-item-command (get-available-items rid))))
    (if ( > (length (get-closed-room rid)) 0)
             (cons 'open t)        
             t)))


; returns a command depending on the item in the room
(define (map-item-command item)
  (if (eq? item 'closed) 
      'open
      (when(eq? item 'key)
          'take)))

;; process the given command in the given room, updates rooms hash table if necessary
(define (process-command cmd rid)
  (cond     
    ;; open the door
    ((equal? cmd 'open)
     (if (>(length (get-closed-room rid)) 0)
         (if(> (player-attribute 'keys) 0)
             (begin           
           (let((target-room (car (get-closed-room rid))))
            (hash-table-set! roomsdb target-room (change-room-item (get-room target-room) 'closed '(closed #f)))
            (player-add-value 'keys -1)
           (describe 'open)))
         (describe 'no-key))
         (describe 'open-error)))
    
    ;; take the key in the room
    ((equal? cmd 'take)
     (if (get-room-item rid 'key)
         (begin
           (hash-table-set! roomsdb rid (change-room-item (get-room rid) 'key '(key #f)))
           (player-add-value 'keys 1)
           (describe 'take))
         (describe 'take-error)))
      
    ;; invalid selection
    (else
     (printf "invalid command"))))

;; returns a new list by changing item with new value in the given list
(define (change-room-item list item new-value)
 (cond
  ((null? list) (quote()))
  (else (cond
        ((eq? (caar list) item)
         (cons new-value (cdr list)))
         (else (cons (car list)
                (change-room-item (cdr list) item new-value)))))))  

;; start the game from given room
(define (startgame room-id)
  (describe 'welcome)
  (let loop ((rid room-id) (echo #t))
    (printf "\n")
    
    ;; print out player items
    (player-status)    
    
    ;; when you reach the destination
    (when (get-room-item rid 'end)
        (begin
          (describe 'found_it)
          (exit)))
    
    ;; print room description and available items
    (when echo
      (printf ">>[  ~a  ]<<\n" (get-room-description rid))
      (map (lambda (y) (describe y)) (get-available-items rid)))
    
    
    
    ;; print out all available commands
    (print-available-commands rid)    
    (printf "> ")
    
    ;; read a character, convert it to string and then lower case and then convert back to a symbol
    ;; the case of characters in string does not affect the hash value produced
    (let ((input (string->symbol (string-downcase (symbol->string (read))))))
      
      ;; check if given command is in available directions
      (if (member input (get-available-directions rid))
          (let ((direction (lookup directions rid input)))
            (if (zero? direction)
                (loop rid #f)
                
                    ;; check if the destination room is closed    
                    ((if (get-room-item direction 'closed)
                        (begin
                          (describe 'closed)
                          (loop rid #f))
                        
                        ;; if it is not closed, go to the destination
                        (loop direction #t)))))
          
          ;; check if the command is in the available commands list
          (if (member input (get-available-commands rid))
              (begin 
                (process-command input rid)
                (loop rid #f))
              
              ;; display a message if the command is not valid
              (begin
                (printf " ~a is an Invalid command!\n" input)
                (loop rid #f)))))))
;; start the game from room 1
(startgame 1)

