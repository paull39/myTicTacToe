(ns tictactoe.core
    (:require [reagent.core :as reagent :refer [atom]]
              [cljs.test :refer-macros [deftest is]]))

(enable-console-print!)

(defn new-board [n]
  (vec (repeat n (vec (repeat n 0)))))

(def board-size 3)

(def win-length 3)

(defonce app-state
  (atom {:text "Hello TicTacToe!"
         :board (new-board board-size)
         :game-status :in-progress}))

(defn computer-move [board]
  (let [remaining-spots (for [i (range board-size)
                              j (range board-size)
                              :when (zero? (get-in board [j i]))]
                          [j i])
        move (when (seq remaining-spots)
               (rand-nth remaining-spots))]
    (if move
      (assoc-in board move 2)
      board)))

(deftest computer-move-test
  (is (= [[2]]
         (computer-move [[0]])))
  (is (= [[1]]
         (computer-move [[1]]))))

(defn straight [owner board [x y] [dx dy] n]
  (every? true?
          (for [i (range n)]
            (= (get-in board [(+ (* dx i) x)
                              (+ (* dy i) y)])
               owner))))

(defn win? [owner board n]
  (some true?
        (for [i (range board-size)
              j (range board-size)
              dir [[1 0] [0 1] [1 1] [1 -1]]]
          (straight owner board [i j] dir n))))

(deftest win?-test
  (is (win? 1 [[1]] 1))
  (is (not (win? 1 [[1]] 2)))
  (is (win? 1 [[2 1]
               [1 2]] 2)))

(defn full? [board]
  (every? #{1 2} (apply concat board)))

(defn game-status [board]
  (cond
    (win? 1 board win-length) :player-victory
    (win? 2 board win-length) :computer-victory
    (full? board) :draw
    :else :in-progress))

(defn update-status [state]
  (assoc state :game-status (game-status (:board state))))

(defn check-game-status [state]
  (-> state
      (update-in [:board] computer-move)
      (update-status)))

(defn blank [i j]
  [:rect {:width 0.9
                :height 0.9
                :fill "gold"
                :x i
                :y j
                :on-click
                (fn blank-click [e]
                  (when (= (:game-status @app-state) :in-progress)
                    (swap! app-state assoc-in [:board j i] 1)
                    (if (win? 1 (:board @app-state) win-length)
                      (swap! app-state assoc :game-status :player-victory)
                      (swap! app-state check-game-status))))}])

(defn cross [i j]
  [:g {:stroke "green"
       :stroke-width 0.2
       :stroke-linecap "round"
       :transform
       (str "translate(" (+ 0.5 i) "," (+ 0.5 j) ") "
            "scale(0.35)")}
   [:line {:x1 -1 :y1 -1 :x2 1 :y2 1}]
   [:line {:x1 1 :y1 -1 :x2 -1 :y2 1}]])

(defn circle [i j]
  [:circle
   {:r 0.45
    :fill "red"
    :cx (+ 0.5 i)
    :cy (+ 0.5 j)}])

(defn tictactoe []
  [:center
   [:h1 (:text @app-state)]
   [:h2
    (case (:game-status @app-state)
      :player-victory "You won! "
      :computer-victory "Computer wins. "
      :draw "Draw. "
      "")
    [:button
     {:on-click
      (fn new-game-click [e]
        (swap! app-state assoc
               :board (new-board board-size)
               :game-status :in-progress))}
     "New Game"]]
   (into
     [:svg
      {:view-box (str "0 0 " board-size " " board-size)
       :width 500
       :height 500}]
     (for [i (range board-size)
           j (range board-size)]
       (case (get-in @app-state [:board j i])
         0 [blank i j]
         1 [circle i j]
         2 [cross i j])))])


(reagent/render-component [tictactoe]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  (prn (:board @app-state)))
