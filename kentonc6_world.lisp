;planets
(add-location bluefish you have successfully landed on planet bluefish the planet with the purple fish.)
(add-location p1 you have just landed on planet crystalpineapple planet of the many cats.)
(add-location p2 you have just landed on planet octopusdisco the planet of the dancing koalas.)
(add-location pcrash you have just crash landed on planet sharpblizzard the planet with the heat storms. there is no visible way out.)
(add-location p4 you have just landed on planet hot papaya the planet famous for its delicious but poisonous bananas.)
(add-location p5 you have just landed on planet newdinosaur the planet of lizards. There is vast lush and inhabitable land all around you.) 

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
(add-object steel-pole p2)

;create game-actions
(game-action weld gold-pole hammer-head p1
             (cond ((not (have 'hammer-head)) '(you do not have a hammer-head.))
                   (*hammer-welded* '(you have already welded a hammer.))
                   (t (setf *hammer-welded* 't)
                    '(you have welded the hammer-head to the gold-pole and you now have a useable hammer.))))

(game-action attach steel-pole shovel-head p2
             (cond ((not (have 'shovel-head)) '(you do not have a shovel-head.))
                   (*shovel-attached* '(you have already attached together a shovel.))
                   (t (progn (setf *shovel-attached* 't)
                    (add-object shovel p2)
                   ; (pickup shovel)
                    '(you have attached the shovel-head to the steel-pole and you now have a useable shovel. Pick it up from the ground if you want to keep it.)

))))

(game-action dig shovel land p5
             (cond (*well-dug* '(you have already dug a well.))
                   (t (setf *well-dug* 't)
                      '(you have dug a hole deep enough to hit a water source and create a well!))))

(game-action build brick-pile land p5
             (cond (*house-built* '(you have already built a house.))
                   ((not *hammer-welded*) '(you need a hammer to build your house.))
                   (t (setf *house-built* 't)
                        '(you have built a sturdy house out of bricks!))))

(game-action set-up wood-pile land p5
             (cond ((not *house-built*) '(you need to build a house first.))
                   (*fence-set-up* '(you have already set-up a fence.))
                   (t (setf *fence-set-up* 't)
                     '(you have successfully set up a protective wooden fence around your house.))))