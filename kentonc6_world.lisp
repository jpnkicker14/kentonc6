;planets
(add-location bluefish you have successfully landed on planet bluefish the planet with the purple fish. though you may not be able to eat the fish use this planet as your center if you get lost.)
(add-location p1 you have just landed on the giving planet.);wood bricks
(add-location p2 you have just landed on planet octopusdisco. what you see on the ground does not mean there were hoes but you may want it for another garden tool.);shovel
(add-location pcrash you have just crash landed on planet hollywood. there is no visible way out instead of partying forever. you can type start-over or quit.);crash
(add-location p4 you have just landed on the garden of wheat thins. the lizard seems to be pointing at something on the ground.);poison 
(add-location p5 you have just landed on planet aladdin.);settle 
(add-location p6 you have just landed on planet fifty-one. ever heard of it?)
(add-location p7 you have just landed on planet voi dire. does it really matter?)
(add-location p8 you have just landed on planet mc. what time is it?);get and make hammer

;paths
(add-path earth bluefish up)
(add-path bluefish p1 left right)
(add-path bluefish p2 right left)
(add-path bluefish pcrash forward)
(add-path bluefish p4 back forward)
(add-path bluefish p5 up down)
(add-path p1 p6 forward back)
(add-path p5 p7 left right)
(add-path p7 p8 foward back)

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
(add-object gold-pole p8)
(add-object steel-pole p2)
(add-object apple p7)
(add-object strawberry p6)

;create game-actions
(game-action weld gold-pole hammer-head p8
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
                   ((not *fence-set-up*) '(it looks like there is going to be an asteroid shower soon. maybe you should build a secure shelter first...))
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

(game-action plant seeds land p5
             (cond (*seeds-planted* '(you have already planted your seeds.))
                   ((not *well-dug*) '(you need some kind of source of water first if you want to plant seeds.))
                   ((not (have 'flashlight)) '(your seeds need artificial light to begin growing.))
                   (t (setf *seeds-planted* 't)
                      '(you have successfully planted your seeds and set up artifical lighting with your flashlight so that they can begin growing.))))