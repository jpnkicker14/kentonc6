;planets
(add-location bluefish you have successfully landed on planet bluefish the planet with the purple fish.)
(add-location p1 you have just landed on planet crystalpineapple planet of the many cats.)
(add-location p2 you have just landed on planet octopusdisco the planet of the dancing koalas. There is a giant pond in front of you.)
(add-location pcrash you have just crash landed on planet sharpblizzard the planet with the heat storms. there is no visible way out.)
(add-location p4 you have just landed on planet hot papaya the planet famous for its delicious but poisonous bananas.)
(add-location p5 you have just landed on planet newdinosaur the planet of lizards.) 

;paths
(add-path earth bluefish up)
(add-path bluefish p1 left right)
(add-path bluefish p2 right left)
(add-path bluefish pcrash forward)
(add-path bluefish p4 up down)
(add-path bluefish p5 down up)

;objects on earth
(add-object shovel-head earth)
(add-object screwdriver earth)
(add-object nails earth)
(add-object seeds earth)
(add-object pillow earth)
(add-object hammer-head earth)
(add-object flashlight earth)
(add-object knife earth)
(add-object iPhone earth)
(add-object matches earth)

;objects in other locations
(add-object banana p4)
(add-object brick-pile p1)
(add-object wood-pile p1)
(add-object gold-pole p1)
(add-object giant-can p2)
(add-object steel-pole p2)

;create game-actions
(game-action weld gold-pole hammer-head p1)
             (if (and (have 'hammer-head) (not *hammer-welded*))
                 (progn (setf *hammer-welded* 't)
                        '(you have welded the hammer-head to the gold-pole and you now have a useable hammer.))
               '(you do not have a hammer-head.)))
