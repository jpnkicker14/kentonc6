(defparameter *ID* "Kenton Chun & Amy Takayesu")

;prevents Allegro from chopping off messages
(setf tpl:*print-length* nil)

;allowed commands
(defparameter *allowed-commands* '(look fly pickup inventory help settle start-over))

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

;keeps track of if the shovel has been welded yet
(defparameter *shovel-attached* nil)

;keeps track of if the house has been built yet
(defparameter *house-built* nil)

;keeps track of if the fence has been built yet
(defparameter *fence-set-up* nil)

;keep track of if the well has been dug
(defparameter *well-dug* nil)

;keeps track of it the seeds have been planted
(defparameter *seeds-planted* nil)

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

;allows user to eat edible objects
(defun eat (object)
   ;first check if you have the object
   (if (not (have ',object)) `(you dont have the ,object)
   (progn (cond ((eq object banana) '(you have eaten the poisonous banana! you were warned but now you are dead. you can type start-over or quit.))
               ((eq object apple) '(you have just eaten an apple. it was pretty good. but now you are even more hungry.))
               ((eq object strawberry) '(you have just eaten a strawberry. it was delicious and you wonder how it got there in the first place.))))))
               

;use at the end when user is satisfied with their work done in outer space and want to settle
(defun settle ()
   (if (not (eq *location* p5) '(you cannot settle here. the conditions are not right.)
  (progn (if (not (have 'knife))
      '(you are about to settle peacefully on your new planet when suddenly a giant alien appears out of nowhere! as you search frantically for some kind of weapon to defend yourself it reaches out and touches you with one of its long greasy alien arms and then picks you up and eats you. i guess you should have brought a weapon. colonizer level 0. you can type start-over or quit.)
    (progn (cond ((and *fence-set-up* *well-dug* *seeds-planted*) '(an alien sees that you are trying to settle on his planet and tries to stop you by throwing hot bananas at your face. luckily you were smart enough to bring a knife to defend yourself. congratulations! you have succesfully built shelter and protection and a source of water and a source of food. you will be able to sustain an entire colony here while you watch from afar as earth slowly withers away. colonizer level 5. you can type start-over or quit.))
                 ((and (not *seeds-planted*) *fence-set-up* *well-dug*) '(an alien sees that you are trying to settle on his planet and tries to stop you by talking smack about your grandma. luckily you were smart enough to bring a knife to defend yourself. good job on creating your own shelter and protection and source of water. too bad you will eventually starve to death because you have no food to eat. colonizer level 4. you can type start-over or quit.))
                 ((and (not *well-dug*) *fence-set-up*) '(an alien sees that you are trying to settle on his planet and tries to stop you by challenging you to a dance battle. luckily you were smart enough to bring a knife to defend yourself. good job on creating a shelter and protection. but i dont know what you are going to do without food and water. should have thought about that earlier! colonizer level 3. you can type start-over or quit.))
                 ((and (not *fence-set-up*) *house-built*) '(an alien sees that you are trying to settle on his planet and tries to stop you by jumping up and down ferociously to create giant planet tremors. luckily you were smart enough to bring a knife to defend yourself. good job on creating a shelter! but i think you are going to need more protection than that. also you will either starve or dehydrate yourself to death. have fun with that. colonizer level 2. you can type start-over or quit.))
                 (t '(an alien sees that you are trying to settle on his planet and tries to stop you by excreting digusting alien slime in your general direction. luckily you were smart enough to bring a knife to defend yourself. now you are stuck alone on a strange planet with no shelter protection or food or water. maybe you should have just kept the alien so youd have company. colonizer level 1. you can type start-over or quit.))))))))

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
  (format t "You have reached the list of commands. ~% look: Describes your current location. This includes objects in the room furniture and exit. ~% fly direction: Choose which direction to fly in and your character will do so as long as there is some kind of path in that direction. ~% pickup item: Pick up an item and put it in your inventory. ~% inventory: Lists all of the items that you currently have in your possession. ~% settle: When you are satisfied with your living conditions on your new planet you must choose to settle and your outcome will be determined. ~% start-over: Restart game back on planet earth."))

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

(defun start-over ()
  (setf *location* 'earth)
  (setf *hammer-welded* nil)
  (setf *shovel-attached* nil)
  (setf *house-built* nil)
  (setf *fence-set-up* nil)
  (setf *tank-dunked* nil)
  (setf *well-dug* nil)
  (setf *seeds-planed* nil)
  (setf *objects* '())
  (setf *object-locations* '())
  (load "kentonc6_world.lisp")
  (look)
)
 
(load "kentonc6_world.lisp")