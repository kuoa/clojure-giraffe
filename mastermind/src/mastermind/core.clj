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


(defn code-secret [n]
  "Returns a vector containing n random colors"
  (let [colors [:red, :green, :blue, :yellow, :purple, :cyan]]
    (loop [n n, res []]
      (if (zero? n)
        res
        (recur (dec n) (conj res (rand-nth colors)))))))


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

(defn to-color-keyword
  "Converts the string-answer to a color-keyword vector"
  [text]
  (let [char-seq (re-seq #"[rgbypc]" text)]
    (mapv #(color-char %) char-seq)))

(defn to-color-string
  "Prints a colored version of the answer"
  [color-vector]
  (let [colored-vec (map #(colored-back "  " %) color-vector)]
    (clojure.string/join " " colored-vec)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (to-color-string [:red, :green, :blue, :yellow, :purple, :cyan]))
  (println (to-color-string [:red, :green, :blue, :yellow, :purple, :cyan])))
