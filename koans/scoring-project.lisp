;;; Copyright 2013 Google Inc.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; Greed is a dice game played among 2 or more players, using 5
;;; six-sided dice.
;;;
;;; Each player takes a turn consisting of one or more rolls of the dice.
;;; On the first roll of the game, a player rolls all five dice which are
;;; scored according to the following:
;;;
;;;   Three 1's => 1000 points
;;;   Three 6's =>  600 points
;;;   Three 5's =>  500 points
;;;   Three 4's =>  400 points
;;;   Three 3's =>  300 points
;;;   Three 2's =>  200 points
;;;   One   1   =>  100 points
;;;   One   5   =>   50 points
;;;
;;; A single die can only be counted once in each roll.  For example,
;;; a "5" can only count as part of a triplet (contributing to the 500
;;; points) or as a single 50 points, but not both in the same roll.
;;;
;;; Example Scoring
;;;
;;;    Throw       Score
;;;    ---------   ------------------
;;;    5 1 3 4 1   50 + 2 * 100 = 250
;;;    1 1 1 3 1   1000 + 100 = 1100
;;;    2 4 4 5 4   400 + 50 = 450
;;;
;;; The dice not contributing to the score are called the non-scoring
;;; dice.  "3" and "4" are non-scoring dice in the first example.  "3" is
;;; a non-scoring die in the second, and "2" is a non-score die in the
;;; final example.
;;;
;;; More scoring examples are given in the tests below.
;;;
;;; Your goal is to write the scoring function for Greed.

(defstruct rule
  count1 points)

(defun calculate-die-score (count1 rules)
  (setq points-scored 0)
  (setq count1-remaining count1)
  (dolist (rule1 rules points-scored)
    (multiple-value-bind (q r) (floor count1-remaining (rule-count1 rule1))
      (incf points-scored (* q (rule-points rule1)))
      (setf count1-remaining r)
    )
  )
)

(defun score (&rest dice)
  (let
    (
      (count1-hash (make-hash-table))
      (rules-hash (make-hash-table))
    )
    (dolist (die dice)
      (setf (gethash die count1-hash) (1+ (gethash die count1-hash 0)))
    )
    (setf (gethash 1 rules-hash) (list (make-rule :count1 3 :points 1000) (make-rule :count1 1 :points 100)))
    (setf (gethash 2 rules-hash) (list (make-rule :count1 3 :points 200)))
    (setf (gethash 3 rules-hash) (list (make-rule :count1 3 :points 300)))
    (setf (gethash 4 rules-hash) (list (make-rule :count1 3 :points 400)))
    (setf (gethash 5 rules-hash) (list (make-rule :count1 3 :points 500) (make-rule :count1 1 :points 50)))
    (setf (gethash 6 rules-hash) (list (make-rule :count1 3 :points 600)))
    (loop for k being the hash-keys of count1-hash using (hash-value v) sum (calculate-die-score v (gethash k rules-hash)))
  )
)

(define-test score-of-an-empty-list-is-zero
  (assert-equal 0 (score)))

(define-test score-of-a-single-roll-of-5-is-50
  (assert-equal 50 (score 5)))

(define-test score-of-a-single-roll-of-1-is-100
  (assert-equal 100 (score 1)))

(define-test score-of-multiple-1s-and-5s-is-the-sum-of-individual-scores
  (assert-equal 300 (score 1 5 5 1)))

(define-test score-of-single-2s-3s-4s-and-6s-are-zero
  (assert-equal 0 (score 2 3 4 6)))

(define-test score-of-a-triple-1-is-1000
  (assert-equal 1000  (score 1 1 1)))

(define-test score-of-other-triples-is-100x
  (assert-equal 200  (score 2 2 2))
  (assert-equal 300  (score 3 3 3))
  (assert-equal 400  (score 4 4 4))
  (assert-equal 500  (score 5 5 5))
  (assert-equal 600  (score 6 6 6)))

(define-test score-of-mixed-is-sum
  (assert-equal 250  (score 2 5 2 2 3))
  (assert-equal 550  (score 5 5 5 5)))
