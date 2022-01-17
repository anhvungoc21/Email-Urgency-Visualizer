#lang racket

(require csc151)
(require 2htdp/image)
(require csc151/rex)
(require rackunit)
(require rackunit/text-ui)

; text-visualization.rkt
;
; CSC-151 Fall 2021
; Mini Project 4
; Author: Anh Vu
; Date: 2021-10-6

;;; (extract-words-downcase str) -> list?
;;;  str : str?
;;; Downcases the input string. Split the input string into words
;;; according to the splitter.
(define extract-words-downcase
  (let ([splitter (rex-repeat (rex-any-of (rex-char-set ",;:.!?()@#$%^&~=_\"<>/[]{}|")
                                          (rex-string " ")
                                          (rex-string "\n")
                                          (rex-string "\r")
                                          (rex-string "\t")))])
    (lambda (str)
      (rex-split-string splitter (string-downcase str)))))

;;; (extract-sentences str) -> integer?
;;;  str : str?
;;; Split the input string into sentences according to the splitter.
;;; Return the number of sentences identified
(define tally-sentences
  (let ([splitter (rex-any-of (rex-string "\r\n")
                              (rex-concat (rex-char-set ".!?")
                                          (rex-repeat-0 (rex-string " "))))])
    (lambda (str)
      (length (rex-split-string splitter (string-normalize-spaces str))))))

;;; (tally-urgency-words str) -> integer?
;;;  str : str?
;;; Count the number of words from "urgent-words.txt" that are
;;; present in the input string
(define tally-urgent-words
  (let ([urgency-word-list (file->lines "urgent-words.txt")])
    (lambda (str)
      (let ([tally-word (section tally-value (extract-words-downcase str) <>)])
        (reduce + (map tally-word urgency-word-list))))))

;;; (ratio-urgent-to-sentences str) -> number?
;;;  str : str?
;;; Find the ratio of urgent words to the number sentences
(define ratio-urgent-to-sentences
  (lambda (str)
    (/ (tally-urgent-words str) (tally-sentences str))))

;;; (visualized-urgent-level str) -> image?
;;;  str : str?
;;; Creates a rectangle with sizes and colors based on the data
;;; analyzed about the input string
(define visualized-urgent-level
  (lambda (str)
    (let ([percent (* 50 (ratio-urgent-to-sentences str))])
      (if (<= percent 100)
          (let* ([red-ish (make-color 255 (round (- 255 (* 255 (/ percent 100)))) 0)]
                 [green-ish (make-color 0 (round (- 255 (* 255 (/ percent 100)))) 0)]
                 [fixed-bar-num (if (< (/ percent 25) 1)
                                    0
                                    (floor (/ percent 25)))]
                 [width-vary (if (= fixed-bar-num 0)
                                 (/ (* percent 300) 100)
                                 (/ (* (- percent (* fixed-bar-num 25)) 300) 100))]
                 [bar-fixed (section rectangle 65 90 'solid <>)]
                 [bar-vary (section rectangle width-vary 90 'solid <>)]
                 [green-fixed (bar-fixed green-ish)]
                 [red-fixed (bar-fixed red-ish)]
                 [green-vary (bar-vary green-ish)]
                 [red-vary (bar-vary red-ish)]
                 [space (rectangle 10 90 'solid "white")])
            (cond
              [(< percent 25)
               green-vary]
              [(= percent 25)
               green-fixed]
              [(< percent 50)
               (beside green-fixed space green-vary)]
              [(= percent 50)
               (beside green-fixed space green-fixed)]
              [(< percent 75)
               (beside red-fixed space red-fixed space red-vary)]
              [(= percent 75)
               (beside red-fixed space red-fixed space red-fixed)]
              [(< percent 100)
               (beside red-fixed space red-fixed space red-fixed space red-vary)]
              [else
               (beside red-fixed space red-fixed space red-fixed space red-fixed)]))
          (text "OVERLOAD" 50 "red")))))

;;; An image of a battery, used later as a skeleton for the final image
(define battery
  (let* ([black-outline (pen "black" 5 'solid 'round 'round)]
         [battery-body (rectangle 300 100 'outline black-outline)]
         [battery-cap (overlay (rectangle 15 40 'outline black-outline)
                               (rectangle 15 40 'solid "white"))])
    (beside battery-body battery-cap)))

;;; (urgency-bars str) -> image?
;;;  str : str?
;;; Overlay the urgency bars created by `visualized-urgent-level`
;;; on a white rectangle for later alignment purposes
(define urgency-bars
  (lambda (str)
    (overlay/align 'left
                   'center
                   (visualized-urgent-level str)
                   (rectangle 290 90 'solid "white"))))

;;; (urgency-image str) -> image?
;;;  str : str?
;;; Overlay the urgency bars created by `urgency-bars` on to
;;; the skeleton of the battery to create the final desired image
(define urgency-image
  (lambda (str)
    (overlay/offset (urgency-bars str)
                    8
                    0
                    battery)))
