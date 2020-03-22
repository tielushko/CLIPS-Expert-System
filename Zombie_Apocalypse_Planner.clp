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
    (slot user_weapon)
)

(defclass WEAPON
    (is-a USER)
    (role concrete)
    (slot weapon_type)
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

(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then yes 
       else no))

;;;***************
;;;* OBJECT DEFS *
;;;***************

(definstances ZOMBIE-INSTANCES
    (z of ZOMBIE)
    ;(crawling of ZOMBIE (walking no))
    ;(rage of ZOMBIE (walking yes)(is_fast yes))
    ;(walker of ZOMBIE (walking yes)(is_fast no))
)

(definstances SURVIVOR-INSTANCES
    (user of SURVIVOR)
)

;(definstances WEAPON-INSTANCES
;    (sword of WEAPON (weapon_type edged))
;    (bat of WEAPON (weapon_type blunt))
;    (bow of WEAPON (weapon_type silent-ranged))
;    (gun of WEAPON (weapon_type loud-ranged))
;)

;;;***************
;;;*  QUESTIONS  *
;;;***************

(defrule ask_moving (declare (salience 10000))
    ?ins <- (object (is-a SURVIVOR))
=>
    (send ?ins put-on_the_move (ask-question "Are you on the move(yes/no)? " yes no))))
    ;(assert (on_the_move(yes-or-no-p "Are you on the move (yes/no)?"))))

(defrule ask_strength ""
	?ins <-(object (is-a SURVIVOR))
=> 
    (send ?ins put-strong (ask-question "Can you bench at least 135 lbs (yes/no)? " yes no))))
	;(assert (strong (yes-or-no-p "Can you bench at least 135 lbs (yes/no)?"))))

(defrule ask_stamina ""
	?ins <- (object (is-a SURVIVOR))
=> 
    (send ?ins put-stamina (ask-question "Can you run a mile in less than 8 minutes (yes/no)? " yes no))))
	;(assert (stamina (yes-or-no-p "Can you run a mile in less than 8 minutes (yes/no)?"))))

(defrule ask_walk ""
	?ins <- (object (is-a ZOMBIE))
=> 
    (send ?ins put-walking (ask-question "Can the zombies walk (yes/no)? " yes no))))
	;(assert (walking (yes-or-no-p "Can the zombies walk (yes/no)?"))))

(defrule ask_speed ""
	?ins <- (object (is-a ZOMBIE) (walking yes))
=> 
     (send ?ins put-is_fast (ask-question "Do the zombies run (yes/no)? " yes no))))
	;(assert (is_fast (yes-or-no-p "Do the zombies run (yes/no)?"))))

;;;***************
;;;*    RULES    *
;;;***************

(defrule if_sedentary (declare (salience 100))
    ?ins <- (object (is-a SURVIVOR) (on_the_move no))
=> 
    (send ?ins put-user_weapon gun)
)

(defrule is_fit
    ?ins <- (object (is-a SURVIVOR) (strong yes) (stamina yes))
=>
    (send ?ins put-sur_type fit)
)

(defrule is_powerful
    ?ins <- (object (is-a SURVIVOR) (strong yes) (stamina no))
=>
    (send ?ins put-sur_type power)
)

(defrule has_staying_power
    ?ins <- (object (is-a SURVIVOR) (strong no) (stamina yes))
=>
    (send ?ins put-sur_type runner)
)

(defrule is_a_couch_potato
    ?ins <- (object (is-a SURVIVOR) (strong no) (stamina no))
=>
    (send ?ins put-sur_type couch_potato)
)


(defrule is_walker
    ?ins <- (object (is-a ZOMBIE) (walking yes) (is_fast no))
=>
    (send ?ins put-z_type walker)
)

(defrule is_sprinter
    ?ins <- (object (is-a ZOMBIE) (walking yes) (is_fast yes))
=>
    (send ?ins put-z_type sprinter)
)

(defrule is_crawler
    ?ins <- (object (is-a ZOMBIE) (walking no))
=>
    (send ?ins put-z_type crawler)
)

;not working yet, but the final version of these functions should be close to this;
;(defrule use_gun(declare (salience -50))
;    (object (is-a SURVIVOR) (sur_type fit) (on_the_move yes))
;    (object (is-a ZOMBIE) (z_type sprinter))
;=>
;    (send user put-user_weapon gun)
;)


(defrule suggest_weapon (declare (salience -100))
    (object (is-a SURVIVOR) (user_weapon ?wep) (sur_type ?stype) (on_the_move ?otm))
    (object (is-a ZOMBIE) (z_type ?zt))
=>
    (printout t "A " ?stype " person who is " ?otm " going up against " 
    ?zt " zombies should use a " ?wep "."  crlf)
)