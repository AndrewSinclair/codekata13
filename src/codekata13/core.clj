(ns codekata13.core
  (:refer-clojure :exclude [char])
  (:use [the.parsatron]))

(defparser eol []
  (choice (>> (char \return) (char \newline))
          (char \return)
          (char \newline)
          (eof)))

(defparser any-string []
  (many1
    (choice
      (letter)
      (digit)
      (char \space)
      (char \tab)
      (char \\)
      (char \*)
      (char \+)
      (char \-)
      (char \/)
      (char \{)
      (char \})
      (char \!)
      (char \~)
      (char \%)
      (char \^)
      (char \&)
      (char \()
      (char \))
      (char \_)
      (char \=)
      (char \[)
      (char \])
      (char \|)
      (char \')
      (char \")
      (char \:)
      (char \;)
      (char \?)
      (char \<)
      (char \>)
      (char \,)
      (char \.)
      (char \`)
      (char \@)
      (char \#)
      (char \$))))

(defparser any-line []
  (let->> [text (any-string)
           _    (eol)]
    (always text)))

(defparser single-line-comment []
  (let->> [start (string "\\\\")
           _     (any-line)]
    (always (apply str start))))

(defparser loc-parser []
  (let->> [line      (either (single-line-comment)
                             (any-line))
           num-lines (either (loc-parser)
                             (eof))]
    (always
      (let [total (if (number? num-lines) num-lines 0)]
        (if (= line "\\\\")
          total
          (inc total))))))

(defn count-lines
  [lines-of-file]
  (run (loc-parser) lines-of-file))

(defn get-file
  [filename]
  (slurp filename))

(defn -main
  "Codekata 13"
  [& args]
  (->>
    (first args)
    get-file
    count-lines
    println))
