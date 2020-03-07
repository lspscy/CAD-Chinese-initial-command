(vl-load-com)
(or *doc* (setq *doc* (vla-get-ActiveDocument (vlax-get-acad-object))))
(or *chinese-text* (setq *chinese-text* (vlr-editor-reactor nil '((:vlr-unknownCommand . chinese-text)))))
;(or *chinese-text* (setq *chinese-text* (vlr-editor-reactor nil '((:vlr-commandEnded . chinese-text)))))

(defun chinese-text(str b / asc)
  (setvar "cmdecho" 0)
  (setq str (strcase  (car b)))
  
  (setq sendcommand-text  nil);清空
  (setq sendcommand-text  (pingyin str));
  (if (/= sendcommand-text nil)
    (progn
      (vla-sendcommand *doc* sendcommand-text)
      (vla-sendcommand *doc* "\n")
    )
  )
  (setq sendcommand-text nil)
  (princ)
)
  
  ;;获取汉字的拼音首字母，返回字母表
(defun pingyin (str / asc lst rt isletter)
  (setq lst (vl-string->list str))
  (setq rt "0")
  (setq isletter 0)
  (while lst
    (if (or (<= 65 (car lst) 90) (<= (car lst) 44))
    (progn
       (setq rt (strcat rt (vl-list->string (list (car lst)))))
       (setq lst (cdr lst))
    )

    (progn
       (setq asc (+ (* (car lst) 256) (cadr lst) -65536)
             lst (cddr lst)
       )
    
       (setq isletter 1)
       (setq rt (strcat rt
         (cond
            ((<= -20319 asc -20284) "A")
            ((<= -20283 asc -19776) "B")
            ((<= -19775 asc -19219) "C")
            ((<= -19218 asc -18711) "D")
            ((<= -18710 asc -18527) "E")
            ((<= -18526 asc -18240) "F")
            ((<= -18239 asc -17923) "G")
            ((<= -17922 asc -17418) "H")
            ((<= -17922 asc -17418) "I")
            ((<= -17417 asc -16475) "J")
            ((<= -16474 asc -16213) "K")
            ((<= -16212 asc -15641) "L")
            ((<= -15640 asc -15166) "M")
            ((<= -15165 asc -14923) "N")
            ((<= -14922 asc -14915) "O")
            ((<= -14914 asc -14631) "P")
            ((<= -14630 asc -14150) "Q")
            ((<= -14149 asc -14091) "R")
            ((<= -14090 asc -13319) "S")
            ((<= -13318 asc -12839) "T")
            ((<= -12838 asc -12557) "W")
            ((<= -12556 asc -11848) "X")
            ((<= -11847 asc -11056) "Y")
            ((<= -11055 asc -10247) "Z")
	    ((= -23636 asc ) ",")
            (t nil)
         )		
       )
    )
    )
  ))
  (if  (= isletter 0)
        (setq rt nil)
        (substr rt 2)
  )
  
)