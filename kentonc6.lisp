; Assignment 6
; Author: Kenton Chun
; Author: Amy Takayesu
; a lot of code comes from the Land of Lisp book

(defparameter *ID* "Kenton Chun & Amy Takayesu")

;prevents Allegro from chopping off messages
(setf tpl:*print-length* nil)

;allowed commands
(defparameter *allowed-commands* '(look fly pickup inventory help))

;nodes for the scenery stored in a list
(defparameter *nodes* '((earth (you are in the space station on planet earth.))))

;edges will store the paths and the choices you have from that location
(defparameter *edges* '())

;objects on the ground parmater 
(defparameter *objects* '())

; the list of where the objects are located
(defparameter *object-locations* '())

;the default location 
(defparameter *location* 'earth)

;keeps track of if the hammer has been welded yet
(defparameter *hammer-welded* nil)

;function that describes the location
;this function will use the assoc to find the location based from the nodes
;then cadr will return the description  
(defun describe-location (location nodes)
   (cadr (assoc location nodes)))

;returns the descripiton based on the edge
(defun describe-path (edge)
  `(you can go ,(cadr edge) from here.))

;will call the describe path if there is more than one possible path
;the map car will be applyed to every member of the list  
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;the function that will return the objects at a given location
(defun objects-at (loc objs obj-loc)
   (labels ((is-at (obj)
              (eq (cadr (assoc obj obj-loc)) loc)))
       (remove-if-not #'is-at objs)))

;function to describe the objects visible at a given location
(defun describe-objects (loc objs obj-loc)
   (labels ((describe-obj (obj)
                `(you see a ,obj on the ground outside the spaceship.)))
      (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

;will return all of the parameters
(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

;takes a direction and moves to that location if it's legal
(defun fly (direction)
  (labels ((correct-way (edge)
             (eq (cadr edge) direction)))
    (let ((next (find-if #'correct-way (cdr (assoc *location* *edges*)))))
      (if next 
          (progn (setf *location* (car next)) 
                 (look))
          '(you cannot go that way.)))))

;tracks the location of objects 
(defun pickup (object)
  ;first check if you are at earth as that will have a limit on the amount you can pickup 
  (if(eq *location* 'earth)
      ;check if less than 5
      (progn(if(<(list-length(objects-at 'body *objects* *object-locations*))5)
		(progn(push (list object 'body) *object-locations*)
	      `(you are now carrying the, object))
	      '(you cannot get that.)))
    ;if its not on earth
    ;check if you can pick it up
    (if(member object (objects-at *location* *objects* *object-locations*))
	(progn(push(list object 'body) *object-locations*)
	      `(you are now carrying the ,object))
      '(you cannot get that.))
))	    

;keeps track of the objects that were picked up 
(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

;used to check if you have an object
(defun have (object) 
    (member object (cdr (inventory))))

;starts the special game REPL
(defun game-repl ()
    (let ((cmd (game-read)))
        (unless (eq (car cmd) 'quit)
            (game-print (game-eval cmd))
            (game-repl))))

;reads the commands 
(defun game-read ()
    (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
         (flet ((quote-it (x)
                    (list 'quote x)))
             (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

;calls the commands
(defun game-eval (sexp)
    (if (member (car sexp) *allowed-commands*)
        (eval sexp)
        '(i do not know that command.)))

;alters the output for game-print
(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eql item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

;prints the result in the custom REPL
(defun game-print (lst)
    (princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string lst)) 'list) t nil) 'string))
    (fresh-line))

;prints out descriptions of available commands
(defun help ()
  (format t "You have reached the list of commands. ~% look: Describes your current location. This includes objects in the room furniture and exit. ~% walk direction: Choose which direction to walk in and your character will do so as long as there is some kind of an exit in that direction. ~% pickup item: pick up an item and put it in your inventory. ~% have item: Checks if you have an item in your possession. ~% inventory: Lists all of the items that you currently have in your possession."))

;macro that creates an action that can be performed if the user is in the correct location and has the correct objects
(defmacro game-action (command subj obj place &body body)
  `(progn (defun ,command (subject object)
            (if (and (eq *location* ',place)
                     (eq subject ',subj)
                     (eq object ',obj)
                     (have ',subj))
                ,@body
            '(i cant ,command like that.)))
          (pushnew ',command *allowed-commands*)))

;uses the game-action macro to allow the user to splash the bucket of water on the wizard if they have a bucket, are in the living-room, and the bucket has been successfully filled with water.
;macro that allows user to add a new object, using the parameters object(name of object) and the location (loca tion of object).
;the object must not already exist, and the location must already exist 
(defmacro add-object (object location &body body)
;checks if the object already exists
  `(progn (if(and(not(member ',object *objects* :test 'equal))
                 ;checks if the location already exists
                       (member ',location (mapcar #'car *nodes*)))
                    ,@body
            ;adds location and object names to the defparameters
                  (progn
                    (pushnew ',object *objects*)
                    (pushnew '(,object ,location) *object-locations*) '(sucessfully added object.))
                  ;error statement if the location does not exist or if the object already exists.
                        '(try again. the object already exists or the location does not exist.)) 
            )
)

;macro allows user to add a new location, using the parameters location (name of location) and a description of the location.
;the location must not already exist.
(defmacro add-location (location &rest desc)
;checks if location already exists
  `(progn (if(not(member ',location (mapcar #'car *nodes*))) 
              ;adds location and description to defparameter *nodes*
     (progn(pushnew '(,location (,@desc)) *nodes*)'(successfully added object.))
     ;error message if the location already exists.
     '(try again. the location already exists.)))
)

;macro allows user to add a new path, using the parameters portal (type of path), from(starting location), to (ending location), din (direction from starting location to ending location), and optional dback (direction from ending location to starting location)
;both locations must exist, and both locations cannot already have other locations in the designated positions. the two locations cannot already have a path between them.
(defmacro add-path (start fin din &optional dback)
  `(progn(add-path-helper ,start ,fin ,din)
  ;checks if path is two-directional
  (if(not(equal ',dback nil))
          (add-path-helper ,fin ,start ,dback)))
)


(defmacro add-path-helper(start fin dir)
  ;checks if both locations exist                                                                                                                       
  `(if(and(member ',start (mapcar #'car *nodes*))
                   (member ',fin (mapcar #'car *nodes*)))
        ;check if location exist in edges
	     ;if not pushnew location to edges
             (progn(if(not(member ',start (mapcar #'car *edges*)))
                 (pushnew '(,start)*edges*))
	     ;checks if a path already exists between the two locations                                        
	    (if(not(member ',fin (mapcar #'car(cdr(assoc ',start *edges*)))))
                       ;checks if the starting location does not already have a path in the requested direction
                       (if(not(member ',dir (mapcar #'cadr(cdr(assoc ',start *edges*)))))
                           ;adds new path to defparameter *edges*                                                                           
                           (progn (pushnew '(,fin ,dir) (cdr(assoc ',start *edges*))))
                      ;error statement if there is already a location connected to the starting location in the requested direction     
                      '(there is already a location connected to ,fin from ,dir))
             ;error statement if there is already a path connecting the two locations                                                 
             '(path already exists.)))
  ;error statement if one or both or the locations do not exist.                                                                            
  '(one or more of the locations does not exist))
)
 
(load "kentonc6_world.lisp")