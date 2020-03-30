;;;***********************************
;;;    Tyler Brown, Oleg Tielushko    *
;;;                                  *
;;; Title: Zombie Apocalypse Planner *
;;;                                  *
;;; This system will help you decide *
;;;  which weapon to use during the  * 
;;;        zombie apocalypse.        *
;;;                                  *
;;;   Just load the program, press   *
;;;          reset and run.          * 
;;;***********************************        



;;;***************
;;;* CLASS  DEFS *
;;;***************

(defclass ZOMBIE
    (is-a USER)
    (role concrete)
    (slot z_type)
    (slot walking)
    (slot is_fast)
)

(defclass SURVIVOR
    (is-a USER)
    (role concrete)
    (slot sur_type)
    (slot strong)
    (slot stamina)
    (slot on_the_move)
)


;;****************
;;* DEFFUNCTIONS *
;;****************

(deffunction ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)

;;;***************
;;;* OBJECT DEFS *
;;;***************

;Keeps track of the zombie's attributes
(definstances ZOMBIE-INSTANCES
   (z of ZOMBIE)
)

;Keeps track of the survivor's (user's) attributes
(definstances SURVIVOR-INSTANCES
   (user of SURVIVOR)
)

;;;***************
;;;*  QUESTIONS  *
;;;***************

(defrule ask_moving (declare (salience 10000))
   ?ins <- (object (is-a SURVIVOR))
=>
   (send ?ins put-on_the_move (ask-question "Are you on the move(yes/no)? " yes no)))
)

(defrule ask_strength "" (declare (salience 300))
	?ins <-(object (is-a SURVIVOR)(on_the_move yes))
=> 
   (send ?ins put-strong (ask-question "Can you bench at least 135 lbs (yes/no)? " yes no)))
)

(defrule ask_stamina "" (declare (salience 300))
	?ins <- (object (is-a SURVIVOR)(on_the_move yes))
=> 
   (send ?ins put-stamina (ask-question "Can you run a mile in less than 8 minutes (yes/no)? " yes no)))
)

(defrule ask_walk "" (declare (salience 300))
	?ins <- (object (is-a ZOMBIE))
=> 
   (send ?ins put-walking (ask-question "Can the zombies walk (yes/no)? " yes no))))
)

(defrule ask_speed "" (declare (salience 300))
	?ins <- (object (is-a ZOMBIE) (walking yes))
=> 
   (send ?ins put-is_fast (ask-question "Do the zombies run (yes/no)? " yes no))))
)

;;;***************
;;;*    RULES    *
;;;***************

;classifies the user as sedentary and assigns their weapon
(defrule if_sedentary (declare (salience 100))
   ?ins <- (object (is-a SURVIVOR) (on_the_move no))
=> 
   (assert (weapon_type gun))
)

(defrule suggest_sed (declare (salience 100))
   (object (is-a SURVIVOR)(on_the_move no))
   (weapon_type ?wep)
=>
   (printout t "A person who is in a base should use a " ?wep "." crlf)
)

;classifies the user as fit
(defrule is_fit
   ?ins <- (object (is-a SURVIVOR) (strong yes) (stamina yes))
=>
   (send ?ins put-sur_type fit)
)

;classifies the user as powerful
(defrule is_powerful
    ?ins <- (object (is-a SURVIVOR) (strong yes) (stamina no))
=>
    (send ?ins put-sur_type power)
)

;classifies the user as having stamina
(defrule has_staying_power
   ?ins <- (object (is-a SURVIVOR) (strong no) (stamina yes))
=>
   (send ?ins put-sur_type runner)
)

;classifies the user as being a couch potato
(defrule is_a_couch_potato
   ?ins <- (object (is-a SURVIVOR) (strong no) (stamina no))
=>
   (send ?ins put-sur_type couch_potato)
)

(defrule if_couch_potato 
   ?ins <- (object (is-a SURVIVOR) (sur_type couch_potato) (on_the_move yes))
=> 
   (assert(weapon_type gun))
)

;classifies the zombie as a crawling zombie
(defrule is_crawler
   ?ins <-(object (is-a ZOMBIE) (walking no))
=>
   (send ?ins put-z_type crawler)
)

;classifies the zombie as a walking zombie
(defrule is_walker
   ?ins <- (object (is-a ZOMBIE) (walking yes) (is_fast no))
=>
   (send ?ins put-z_type walker)
)

;classifies the zombie as a sprinting zombie
(defrule is_sprinter
   ?ins <- (object (is-a ZOMBIE) (walking yes) (is_fast yes))
=>
   (send ?ins put-z_type sprinter)
)

;if the zombies are running, it suggests a gun
(defrule if_sprinters (declare (salience -25))
   (object (is-a SURVIVOR) (on_the_move yes))
   (object (is-a ZOMBIE) (z_type sprinter))
=> 
   (assert (weapon_type gun))
)

;suggests the weapon for a fit survivor and a walking zombie
(defrule fit_walk(declare (salience -50))
   (object (is-a SURVIVOR) (sur_type fit) (on_the_move yes))
   (object (is-a ZOMBIE) (z_type walker))
=>
   (assert (weapon_type bow))
)

;suggests the weapon for a fit survivor and a crawling zombie
(defrule fit_crawl(declare (salience -50))
   (object (is-a SURVIVOR) (sur_type fit) (on_the_move yes))
   (object (is-a ZOMBIE) (z_type crawler))
=>
   (assert (weapon_type club))
)

;suggests the weapon for a runner survivor and a walking zombie
(defrule run_walk(declare (salience -50))
   (object (is-a SURVIVOR) (sur_type runner) (on_the_move yes))
   (object (is-a ZOMBIE) (z_type walker))
=>
   (assert (weapon_type bow))
)

;suggests the weapon for a runner survivor and a crawling zombie
(defrule run_crawl(declare (salience -50))
   (object (is-a SURVIVOR) (sur_type runner) (on_the_move yes))
   (object (is-a ZOMBIE) (z_type crawler))
=>
   (assert (weapon_type sword))
)

;suggests the weapon for a powerful survivor and a walking zombie
(defrule power_walk(declare (salience -50))
   (object (is-a SURVIVOR) (sur_type power) (on_the_move yes))
   (object (is-a ZOMBIE) (z_type walker))
=>
   (assert (weapon_type sword))
)

;suggests the weapon for a powerful survivor and a crawling zombie
(defrule power_crawl(declare (salience -50))
   (object (is-a SURVIVOR) (sur_type power) (on_the_move yes))
   (object (is-a ZOMBIE) (z_type crawler))
=>
   (assert (weapon_type club))
)

;Prints out the final statement which suggests the weapon
(defrule suggest_weapon (declare (salience -150))
   (object (is-a SURVIVOR) (sur_type ?stype) (on_the_move yes))
   (object (is-a ZOMBIE) (z_type ?zt))
   (weapon_type ?wep)
=>
   (printout t "A " ?stype " type person who is on the move going up against " 
   ?zt " zombies should use a " ?wep "."  crlf)
)