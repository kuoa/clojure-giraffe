;;***************************************;;
;;              MASTERMIND               ;;
;;***************************************;;
;;                                       ;;
;;             Author: kuoa              ;;
;;***************************************;;


(ns mastermind.core
  (:gen-class))


;;***************************************;;
;;              Game Logic               ;;
;;***************************************;;
;; For practice value, no higher order   ;;
;; such as map, filter, reduce have been ;;
;; used in this part. One must learn.    ;;
;;***************************************;;

(def colors [:red, :green, :blue, :yellow, :purple, :cyan])

(defn code-secret [n]
  "Returns a vector containing n random colors"
    (loop [n n, res []]
      (if (zero? n)
        res
        (recur (dec n) (conj res (rand-nth colors))))))


(defn indications [answer, try]
  "Returns a vector of indications, containing :good | :color | :bad"  
  (loop [index 0, res []]
    (if (< index (count answer))
      (let [ha (get answer index), ht (get try index)]
        (cond 
          (= ht ha) (recur (inc index) (conj res :good))
          (some #(= ht %) answer) (recur (inc index) (conj res :color))
          :default (recur (inc index) (conj res :bad))))
      res)))


(defn frequences [answer]
  "Returns a map from distinct items in answer to the number of times they appear."
  (loop [answer answer, res {}]
    (if (seq answer)
      (let [h (first answer), t (rest answer), m-value (get res h)]
        (if m-value
          (recur t (assoc res h (inc m-value)))
          (recur t (assoc res h 1))))
      res)))


(defn freqs-dispo [code, try-indic]
  "Returns a map of available color freq."
  (loop [index 0, res (frequences code)]
    (if (< index (count code))
      (let [color (get code index), state (get try-indic index)]      
        (if (= state :good)
          (recur (inc index) (update res color dec))
          (recur (inc index) res)))
      res)))



(defn filtre-indications [code, try, indic]
  "Returns a vector with corect indications"  
  (loop [index 0, res [], freq (frequences code)]
    (if (< index (count code))
      (let [color (get try index), tag (get indic index)]
        (if (= tag :bad)
          (recur (inc index), (conj res tag), freq)
          (if (> (get freq color) 0)
            (recur (inc index), (conj res tag), (update freq color dec))
            (recur (inc index), (conj res :bad), freq))))
      res)))


;;***************************************;;
;;            Console utils              ;;
;;***************************************;;
;;           Colors and text             ;;
;;***************************************;;

(def color-char (zipmap ["r" "g" "b" "y" "p" "c"]
                        [:red, :green, :blue, :yellow, :purple, :cyan]))

(def color-code (zipmap [:black, :red, :green, :yellow, :blue, :purple, :cyan, :white, :grey]
                        [0, 1, 2, 3, 4, 5, 6, 7, 8]))

(def indic-code (zipmap [:color :bad] [:grey :black]))


(defn colored-text
  "Returns a colored text using escape sequences"
  [text color]
  (str "\033[3" (color color-code) "m" text "\033[0m"))

(defn colored-back
  "Returns a text with a colored background, using escape sequences"
    [text color]
    (str "\033[4" (color color-code) "m" text "\033[0m"))


;;***************************************;;
;;            Text utils                 ;;  
;;***************************************;;
;;     Text input and manipulation       ;;  
;;***************************************;;


(defn get-input
  "Get a line of text and return a clean version without trailing white spaces"
  []
  (clojure.string/trim (read-line)))

(defn valid-move?
  "Checks if the answer has a valid size and valid characters"
  [code-size text]
  (and (= (count text) code-size) (boolean (re-matches #"[rgbypc]+" text))))

(defn game-finished?
  "Checks if the game is finished"
  [indic]
  (every? #(= :good %) indic))

(defn random-color-text
  "Generate a text-string using random colors"
  [text]

  (clojure.string/join (map colored-text text (code-secret (count text)))))

(defn text-to-color-vec
  "Converts the string-answer to a color-keyword vector"
  [text]
  (let [char-seq (re-seq #"[rgbypc]" text)]
    (mapv #(color-char %) char-seq)))

(defn color-vec-to-print
  "Returns a string-colored version of the answer"
  [color-vector]
  (let [colored-vec (map #(colored-back "  " %) color-vector)]
    (clojure.string/join " " colored-vec)))

(defn ind-to-color-vec
  "Returns a colored version of the indication vector.
  Good -> same color, Bad -> black, Color present -> white"
  [answer, indic]
  (loop [i 0, res []]
    (if (< i (count indic))
      (if (= :good (get indic i))
        (recur (inc i) (conj res (get answer i) ))             ;; add color
        (recur (inc i) (conj res ((get indic i) indic-code)))) ;; add color-code
      res)))


;;***************************************;;
;;                  UI                   ;;  
;;***************************************;;
;;             Text Screens              ;; 
;;***************************************;;

(def center "                ")
(def line "_______________________________________________")
(def n "\n")

(defn surround-text [txt]
  (str line n n center txt n line n))

(defn repeat-s [n]
  (clojure.string/join (repeat n " ")))

(def game-screen (str n n (surround-text (random-color-text "MASTER MIND")) n))

(def colors-screen
  (str "Colors : "
       (clojure.string/join " " (map #(colored-text (name %) %) colors)) n))

(def answer-screen
  (str  "Answer example : "
        (clojure.string/join (map #(colored-text (first %) (second %)) color-char)) n n))

(def display-screen (str "Display example : " (color-vec-to-print colors) n))

(def hint-screen
  (str "Hint example :    "
       (color-vec-to-print
        (ind-to-color-vec colors [:good :color :bad :color :good :bad])) n n))

(def meaning-screen
  (str "Meaning :"
       (colored-text "red " :red) "present at the good position" n
       (repeat-s 10) (colored-text "green " :green) "present but different position" n
       (repeat-s 10) (colored-text "blue " :blue) "not present" n
       (repeat-s 10) (colored-text "yellow " :yellow) "present but different position" n
       (repeat-s 10) (colored-text "purple " :purple) "present at the good position" n
       (repeat-s 10) (colored-text "cyan " :cyan) "not present" n))

(def quit-screen
  (str "To quit: " (colored-text "quit" :blue) n))

(defn size-screen [code-size] 
  (str (surround-text (random-color-text "LET'S PLAY")) n
       "Code size  : " (colored-text code-size :red)))

(defn print-start-screen
  "Start screen string"  
  [code-size]
  (println game-screen colors-screen answer-screen
           display-screen hint-screen meaning-screen
           quit-screen (size-screen code-size)))

(defn print-valid-move [move]
  (println (repeat-s 12)
           (color-vec-to-print (text-to-color-vec move))))

(defn print-invalid-move []
  (println (colored-text "\nInvalid move" :red)))

(defn print-attempt [attempt, number]
  (println "Attempt : " number (color-vec-to-print attempt)))

(defn print-guess []
  (print "Your guess : ") (flush))

(defn print-answer [answer]
  (println "Answer :    " (color-vec-to-print answer)))

(defn print-game-end []
  (println (surround-text (random-color-text "WELL DONE!"))))


(defn prompt-move 
  "Prompt a move"
  [code-size answer]
  (print-guess)
  (let [input (get-input)]
    (cond
      (= "quit" input) (System/exit 0)
      (= "answer" input)              ;; cheaters gonna' cheat xD
      (do (print-answer answer)
          (prompt-move code-size answer))
      :default    
      (if (valid-move? code-size input)
        (do
          (print-valid-move input)
          (text-to-color-vec input))
        (do 
          (print-invalid-move)
          (prompt-move code-size answer))))))


(defn game-loop
  "Main game loop"
  []
  (print-start-screen 5)
  (let [size 5, answer (code-secret size)]
    ;;(print-answer answer)
    (loop [stop false, nb-attempts 1]
      (if (not stop)
        (let [try (prompt-move size answer),
              indic (filtre-indications answer try (indications answer try))
              attempt (ind-to-color-vec answer indic)]
          (do
            (print-attempt attempt nb-attempts)
            (recur (game-finished? indic) (inc nb-attempts))))))
    (print-game-end)))

(defn -main
  "Main program"
  [& args] 
  (game-loop))
