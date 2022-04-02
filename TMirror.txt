;;;*********************************************;;;
;;;            FUNCTION:TMirror			;;;
;;;	Written by: Irving N. 12/19		;;;
;;; ------------------------------------------- ;;;
;;; This function will mirror items horizontally;;;
;;; 	and switch the horzontal justifications ;;;
;;;	of any TEXT objects. 			;;;
;;; --------------------------------------------;;;
;;;*********************************************;;;

(defun C:TMirror ()


(defun *error* ()
       (if os
	 (setvar 'osmode os)
	);if
);defun
(vl-load-com)

(setq ss_all (ssadd))
(prompt "Select objects:")
(terpri)
(while (< (sslength ss_all) 1)
	(setq ss_all (ssget))
  	(if (< (sslength ss_all) 1)
	  	(progn
		  	(prompt "No items selected. Please try again.")
		  	(terpri)
		);progn
	);if
);while

;Assoc List to convert current justification DXF codes to mirrored codes
(setq	init_lst (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)
	just_str_lst (list "L" "C" "R" "ALIGNED" "M" "FIT" "TL" "TC" "TR" "ML" "MC" "MR" "BL" "BC" "BR")
	exclude_just_lst (list "C" "ALIGNED" "M" "FIT" "TC" "MC" "BC")
	assoc_lst (list 2 1 0 3 4 5 8 7 6 11 10 9 14 13 12)
  	just_assoc_lst (mapcar '(lambda (x y) (cons x y)) init_lst assoc_lst)
	just_assoc_str_lst (mapcar '(lambda (x y) (cons x y)) init_lst just_str_lst)
	fin_mirr_ss (ssadd)
	curr_just_cons_lst nil
	new_just_cons_lst nil
);setq

(while (ssname ss_all 0)
  	(setq 	curr_ent (ssname ss_all 0)
	      	curr_ent_get (entget curr_ent)
		curr_ent_type (cdr (assoc 0 curr_ent_get))
		curr_vl (vlax-ename->vla-object curr_ent)
	);setq
  	(if
		(= curr_ent_type "TEXT")
	  	(progn
			(setq curr_just (vla-get-alignment curr_vl)
			      new_just_num (cdr (assoc curr_just just_assoc_lst))
			      new_just_str (cdr (assoc new_just_num just_assoc_str_lst))
			      ss_temp (ssadd)
			      curr_just_cons_lst (cons new_just_str curr_just_cons_lst)
			);setq
		  	(if
			  	(not (member new_just_str exclude_just_lst))
			  	(progn
			  		(ssadd	curr_ent ss_temp)
					(command "._justifytext" ss_temp "" new_just_str)
					(setq new_just_cons_lst (cons new_just_str new_just_cons_lst)
					      reset_original_ent_just_lst (cons (list curr_ent (cdr (assoc curr_just just_assoc_str_lst))) reset_original_ent_just_lst)
					);setq
				);progn
			  	(progn
				  	(setq new_just_cons_lst (cons new_just_str new_just_cons_lst))
				);progn
			);if
		);progn
	);if
  	(ssadd curr_ent fin_mirr_ss)
  	(ssdel curr_ent ss_all)
);while

(setq 	single_mirr_pt (getpoint "Select location of vertical mirror line:")
	second_auto_mirr_pt (list (car single_mirr_pt) (+ (cadr single_mirr_pt) 1) 0.0)
);setq
  
(initget "Yes No")

(if (setq ans (getkword "Erase source objects? [Yes/No] <Yes>:"))
  	(princ)
  	(setq ans "Yes")
);if
(setq os (getvar 'osmode))
(setvar 'osmode 0)
(if (= ans "Yes")
	(command "._mirror" fin_mirr_ss "" single_mirr_pt second_auto_mirr_pt "_Y");
  	(progn
  		(command "._mirror" fin_mirr_ss "" single_mirr_pt second_auto_mirr_pt "_N");
	  	(foreach txt reset_original_ent_just_lst
		  	(setq reset_ss (ssadd))
		  	(ssadd (car txt) reset_ss)
		  	(command "._justifytext" reset_ss "" (cadr txt))
		);foreach
	);progn
);if

(setvar 'osmode os)

(princ)
);defun