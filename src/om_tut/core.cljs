(ns ^:figwheel-always om-tut.core
    (:require[om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload

(def app-state
  (atom
    {:grid
     [
     [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ]
     [0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ]
     [0 0 0 1 0 1 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ]
     [0 0 0 0 1 1 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ]
     [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ]
     [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ]
     [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ]
     [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ]
     [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ]
     [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ]
     [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ]
     [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ]
     [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ]
     [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ]
     [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ]
     ]
     }
    )
)

(defn inBounds [row column data]
  (and (>= row 0) (>= column 0) (< row (count data)) (< column (count (first data)))))

(defn cellValue [row column data]
  (cond 
    (inBounds row column data) (nth (nth data row) column)
    :else 0
  )
)

(defn countLiveNeighbors [row column data]
  (def value (+ 
    (cellValue (+ row 1) column data)
    (cellValue (- row 1) column data)
    (cellValue row (+ column 1) data)
    (cellValue row (- column 1) data)
    (cellValue (+ row 1) (+ column 1) data)
    (cellValue (+ row 1) (- column 1) data)
    (cellValue (- row 1) (+ column 1) data)
    (cellValue (- row 1) (- column 1) data)
  ))
  value
)

(defn calcCell [row column cellValue data]
  (def live (countLiveNeighbors row column data))
  (cond (or (= live 3) (= (+ cellValue live) 3)) 1
    :else 0)	
)

(defn tick [data]
	(def rowcolumns (:grid data))
    (swap! app-state assoc :grid
    	(map-indexed 
    		(fn [row rowData] (map-indexed (fn [column cellData] (calcCell row column cellData rowcolumns)) rowData)) rowcolumns
    	)
    )
)

(defn game-cell [data owner]
	(reify
		om/IRender
		(render [this]
			(cond 
				(= data 1) 
					(dom/span #js {} (dom/img #js {:src "https://encrypted-tbn2.gstatic.com/images?q=tbn:ANd9GcRvQoG8Qdi9c6WOoIwEiaY39WxMqRRhQAQ6IZ-8dBlMRRMIPKtWOom7nQ" :className "cellon cell"}))
				:else
					(dom/span #js {} (dom/img #js {:src "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAIsAAACLCAMAAABmx5rNAAAAA1BMVEUAAIBKwr76AAAAKklEQVR4nO3BMQEAAADCoPVPbQsvoAAAAAAAAAAAAAAAAAAAAAAAAICnAUwEAAH1uApbAAAAAElFTkSuQmCC" :className "cellon cell"}))
			)
		)
	)
)

(defn game-row [data owner]
  (reify
    om/IRender
	  (render [this]
		(apply dom/div #js {} 
			(om/build-all game-cell data)))))

(defn grid-view [data owner]
  (reify
    om/IRender
  	  (render [this]
  	    (dom/div nil
  	      (dom/button #js {:onClick #(tick data) :className "tickbutton"} "Tick")
  	      (apply dom/div nil
  		    (om/build-all game-row (:grid data)))))))

(om/root grid-view app-state
  {:target (. js/document (getElementById "contacts"))})


