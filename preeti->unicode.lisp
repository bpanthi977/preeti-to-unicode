;;;; preeti->unicode.lisp
(in-package #:preeti->unicode)

(defparameter *conversion-list*  (list "` 1 2 3 4 5 6 7 8 9 0 - = q w e r t y u i o p [ ] \\ a s d f g h j k l ; ' < z x c v b n m , . / ~ ! @ # $ % ^ & * ( ) _ + Q W E R T Y U I O P { } | A S D F G H J K L : \" > Z X C V B N M < > ? ç ¿ ? ª § ° ¶ Ë Ì Í Î ‹ • ˆ ß Ø Ý å Ù Û Œ « Ö Ú £ ÷ ¥ œ qm Qm pm em km 0f if If ® “ Af Sf Df Gf Jf Kf :f Wf Ef Rf Tf Yf Uf Zf Xf Vf Nf ˆf cf "
	     "ञ ज्ञ द्द घ द्ध छ ट ठ ड ढ ण् ( . त्र ध भ च त थ ग ष् य उ ृ े ् ब क म ा न ज व प ि स ु ? श ह अ ख द ल m , । र ञ् १ २ ३ ४ ५ ६ ७ ८ ९ ० ) ं त्त ध् भ् च् त् थ् ग् क्ष् इ ए र् ै ्र ब् क् म् ँ न् ज् व् प् ी स् ू श्र श् ह् ऋ ख् द्य ल् ः ? श्र रु ॐ रू रु ङ ट्ट ड्ढ ठ्ठ ङ्ग न्न ङ्क ङ्ख ङ्घ ड्ड फ् द्म ्य ट्ठ द्व ; ! त्त् ्र = ' घ् / र् त्र् क्र क्त ऊ झ फ ण ष क्ष र ँ ब क म न ज प स ध भ च त थ ग श ह ख ल फ आ "))
(Defparameter *base-letter* "` 1 2 3 4 5 6 7 8 9 0 q w e r t y u i o p a s d g h j k ; z x c v b n  / ® ~ Q W E R TY U I O P  A S D G H J K : > Z X C V B N ? ç ¿ ª § ° ¶ Ë Ì Í Î ‹ • ˆ ß Ý å Œ £ ¥")

(defparameter *half-letter* "0 i ~ W E R T Y U I A S D G H J K : Z X V N Œ £ ¥ œ ˆ ")
(defparameter *full-letter* "å Ý ß • ‹ Î Í Ì Ë ¶ ° § ª ¿ ç ? B C > P O Q / ® n b v c x z ; k j h g d s a p o u y t r e w q 9 8 7 6 5 4 3 2 1 ` ")

;; Suffix and prefixes
(defparameter *fix* "M \" L F } + ' l f \ ] [") 
(defparameter *prefix* "l ")
(defparameter *suffix* "M \" L F } + ' ] [ f m { ")
(defparameter *hanuman-chars* "qQpek") ;; these may use m to make different letter
(defparameter *conversion-table* (make-hash-table :test 'equal))

(defun create-table ()
  (let ((preeti (bpu:explode (first *conversion-list*) #\Space))
        (unicode (bpu:explode (second *conversion-list*) #\Space)))
    (loop for item1 in preeti
       for item2 in unicode do
         (setf (gethash item1 *conversion-table*) item2)
         (if (= 1 (length item1))
             (setf (gethash (character item1) *conversion-table*) item2)))))   

(defun translate (token)
  (or (gethash token *conversion-table*) token))

(defmacro add-block ()
  `(if (or pre base suff)
       (progn
         (push (list pre base (reverse suff)) result)
         (setf pre nil base nil suff nil))))

(defun separate-word (word)
  "Separate word into blocks of prefix+base-letter+suffix"
  (let (result final pre base suff)
    (loop for char across word do
         (cond ((find char *prefix*) (add-block)
                (setf pre (list char)))
               ((find char *base-letter*) (if base (add-block))
                (setf base  char))
               ((find char *suffix*) (push char suff))
               (t (if base (push char suff) (setf base char)))))
    (add-block)
    (loop for x in result do (push x final))
    final))

(defun remove-once (char string)
  (let ((p (position char string)))
    (if p
        (bpu:pop-nth p string)
        string)))

(defmacro transpose (char from to)
  `(if (find ,char ,from)
       (progn (setf ,from (remove-once ,char ,from))
              (push ,char ,to))))

(defmacro translate-chars (of)
  `(loop for x in ,of do (format str "~a" (translate x))))

(defun convert-blocks (blocks)
  (with-output-to-string (str)
    (let (carry-over)
    (loop for (pre base suff) in blocks do
       ;;Transpose chars

         (if (or (and (find base *full-letter*) (not (find #\\ suff)))
                 (and (find base *half-letter*) (find #\f suff)))
             (progn (transpose #\l pre suff) ;; इकार
                    (when carry-over
                      (setf suff (bpu:copy-elements carry-over suff))
                      (setf carry-over nil)))
             (transpose #\l pre carry-over))
         (transpose #\{ suff pre) ;; Reph

       ;;Join and split
       ;;1) Hanuman ko puchchar
         (when (and (find #\m suff)
                    (find base *hanuman-chars*))
           (setf base (translate (format nil "~cm" base)))
           (setf suff (remove-once #\m suff)))
       ;;2) half + अ (f) = full
         (when (and (find #\f suff)
                    (or (find base *half-letter*)
                        (and (characterp base) (char= base #\c))))
           (setf base (translate (format nil "~cf" base)))
           (setf suff (remove-once #\f suff)))

         (when (find #\f suff)
           (loop for c in (list #\] #\})
              for rep in (list "ो" "ौ") do
                (when (find c suff)
                  (setf suff (remove-once c (Remove-once #\f suff)))
                  (push rep suff))))
         
;;       (print (list pre base suff))
       ;; Now convert and join
         (translate-chars pre)
         (if base (format str "~a" (translate base)))
         (translate-chars suff))
    str)))

(defun convert-word (word)
  (convert-blocks (separate-word word)))

(defun convert-string (string)
  (bpu:join-strings (mapcar 'convert-word (bpu:explode string #\Space)) " "))

(create-table)



