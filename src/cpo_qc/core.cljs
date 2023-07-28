(ns cpo-qc.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [clojure.set]
            [reagent.core :as r] 
            [reagent.dom :as rdom]
            [reagent.dom.server]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]
            [ag-grid-react :as ag-grid]
            [ag-charts-react :as ag-chart]
            [cljs.pprint :refer [pprint]]))

(defonce db (r/atom {}))

(def app-version "v0.1.0")

(def url-prefix "")


(defn in? 
  "true if coll contains elem"
  [coll elem]  
  (some #(= elem %) coll))


(defn load-illumina-runs []
  ""
  (go
    (let [response (<! (http/get (str url-prefix "data/illumina_runs.json")))]
      (swap! db assoc-in [:illumina-runs] (:body response)))))


(defn load-library-qc [run-id]
  ""
  (go
    (let [response (<! (http/get (str url-prefix "data/library-qc/" run-id "_library_qc.json")))]
      (if (= 200 (:status response))
        (swap! db assoc-in [:library-qc run-id] (:body response))))))


(defn load-plasmid-qc [run-id]
  ""
  (go
    (let [response (<! (http/get (str url-prefix "data/plasmid-qc/" run-id "_plasmid_qc.json")))]
      (if (= 200 (:status response))
        (swap! db assoc-in [:plasmid-qc run-id] (:body response))))))



(defn get-selected-rows [e]
  (map #(js->clj (.-data %) :keywordize-keys true)
       (-> e
           .-api
           .getSelectedNodes)))


(defn run-selected [e]
  (let [previously-selected-run-ids (:selected-run-ids @db)
        currently-selected-run-ids (map :run_id (get-selected-rows e))
        newly-selected-run-ids (clojure.set/difference (set currently-selected-run-ids) (set previously-selected-run-ids))
        currently-selected-runs (filter #(in? currently-selected-run-ids (:run_id %)) (:runs @db))
        newly-selected-runs (filter #(in? newly-selected-run-ids (:run_id %)) (:runs @db))]
    (do
      (doall
       (map load-library-qc newly-selected-run-ids)
       (map load-plasmid-qc newly-selected-run-ids))
      (swap! db assoc-in [:selected-run-ids] currently-selected-run-ids))))


(defn debug-view []
  (let [current-debug (:debug-view @db)
        toggle-debug #(swap! db assoc :debug-view (not current-debug))]
    [:div
     [:button {:on-click toggle-debug} "Toggle Debug View"]
     [:div.debug {:style {:background-color "#CDCDCD" :display (if (:debug-view @db) "block" "none")}}
      [:pre [:code {:style {:font-family "monospace" }}
             (with-out-str (pprint (select-keys @db [:debug-view :selected-run-id])))]]]]))


(defn header []
  [:header {:style {:display "grid"
                    :grid-template-columns "repeat(2, 1fr)"
                    :align-items "center"
                    :height "48px"}}
   [:div {:style {:display "grid"
                  :grid-template-columns "repeat(2, 1fr)"
                  :align-items "center"}}
    [:h1 {:style {:font-family "Arial" :color "#004a87" :margin "0px"}} "CPO QC"][:p {:style {:font-family "Arial" :color "grey" :justify-self "start"}} app-version]]
   [:div {:style {:display "grid" :align-self "center" :justify-self "end"}}
    [:img {:src (str url-prefix "/images/bccdc_logo.svg") :height "48px"}]]])


(defn illumina-runs-table []
  (let [runs (:illumina-runs @db)
        row-data runs]
    [:div {:class "ag-theme-balham"
           :style {}}
     [:> ag-grid/AgGridReact
      {:rowData row-data
       :pagination false
       :rowSelection "multiple"
       :enableCellTextSelection true
       :onFirstDataRendered #(-> % .-api .sizeColumnsToFit)
       :onSelectionChanged run-selected}
      [:> ag-grid/AgGridColumn {:field "run_id" :headerName "Run ID" :minWidth 180 :resizable true :filter "agTextColumnFilter" :floatingFilter true :sortable true :checkboxSelection true :sort "desc"}]]]))


(defn library-qc-table []
  (let [currently-selected-run-ids (:selected-run-ids @db)
        all-library-qc (:library-qc @db)
        selected-runs-library-qc (flatten (vals (select-keys all-library-qc currently-selected-run-ids)))
        num-bases-short-style (fn [params] (do (js/console.log (. params -value)) (if (> 500 (. params -value)) (clj->js {:backgroundColor "#e6675e"}) nil)))
        row-data (->> selected-runs-library-qc
                      (map (fn [x] (update x :num_bases_short #(.toFixed (/ % 1000000 ) 2))))
                      (map (fn [x] (update x :assembly_length #(.toFixed (/ % 1000000 ) 2)))))]
    [:div {:class "ag-theme-balham"
           :style {}}
     [:> ag-grid/AgGridReact
      {:rowData row-data
       :pagination false
       :enableCellTextSelection true
       :onFirstDataRendered #(-> % .-api .sizeColumnsToFit)
       :onSelectionChanged #()
       }
      [:> ag-grid/AgGridColumn {:field "library_id" :headerName "Library ID" :maxWidth 200 :sortable true :resizable true :filter "agTextColumnFilter" :pinned "left" :checkboxSelection false :headerCheckboxSelectionFilteredOnly true :floatingFilter true :sort "desc"}]
      [:> ag-grid/AgGridColumn {:field "inferred_species_name" :headerName "Inferred Species" :headerTooltip "Inferred Species" :maxWidth 200 :sortable true :resizable true :filter "agTextColumnFilter"}]
      [:> ag-grid/AgGridColumn {:field "num_bases_short" :type "numericColumn" :headerName "Megabases Short" :headerTooltip "Megabases from Short Reads" :maxWidth 200 :sortable true :resizable true :filter "agNumberColumnFilter" :cellStyle num-bases-short-style}]
      [:> ag-grid/AgGridColumn {:field "num_bases_long" :type "numericColumn" :headerName "Megabases Long" :headerTooltip "Megabases from Long Reads":maxWidth 200 :sortable true :resizable true :filter "agNumberColumnFilter"}]
      [:> ag-grid/AgGridColumn {:field "assembly_type" :headerName "Assembly Type" :headerTooltip "Assembly Type" :maxWidth 200 :sortable true :resizable true :filter "agTextColumnFilter"}]
      [:> ag-grid/AgGridColumn {:field "assembly_length" :type "numericColumn" :headerName "Assembly Length (Mb)" :headerTooltip "Assembly Length" :maxWidth 220 :sortable true :resizable true :filter "agNumberColumnFilter"}]
      [:> ag-grid/AgGridColumn {:field "assembly_num_contigs" :type "numericColumn" :headerName "Num. Contigs" :headerTooltip "Number of Contigs" :maxWidth 200 :sortable true :resizable true :filter "agNumberColumnFilter"}]
      [:> ag-grid/AgGridColumn {:field "assembly_N50" :type "numericColumn" :headerName "Assembly N50" :headerTooltip "Assembly N50" :maxWidth 200 :sortable true :resizable true :filter "agNumberColumnFilter"}]
      [:> ag-grid/AgGridColumn {:field "mlst_scheme" :headerName "MLST Scheme" :headerTooltip "MLST Scheme" :maxWidth 200 :sortable true :resizable true :filter "agTextColumnFilter"}]
      [:> ag-grid/AgGridColumn {:field "mlst_sequence_type" :headerName "ST" :headerTooltip "MLST Sequence Type" :maxWidth 200 :sortable true :resizable true :filter "agTextColumnFilter"}]
      
      ]]
    ))

(defn plasmid-qc-table []
  (let [currently-selected-run-ids (:selected-run-ids @db)
        all-plasmid-qc (:plasmid-qc @db)
        selected-runs-plasmid-qc (flatten (vals (select-keys all-plasmid-qc currently-selected-run-ids)))
        row-data (->> selected-runs-plasmid-qc
                      identity)]
    [:div {:class "ag-theme-balham"
           :style {}}
     [:> ag-grid/AgGridReact
      {:rowData row-data
       :pagination false
       :enableCellTextSelection true
       :onFirstDataRendered #(-> % .-api .sizeColumnsToFit)
       }
      [:> ag-grid/AgGridColumn {:field "library_id" :headerName "Library ID" :maxWidth 200 :sortable true :resizable true :filter "agTextColumnFilter" :pinned "left" :checkboxSelection false :headerCheckboxSelectionFilteredOnly true}]
      [:> ag-grid/AgGridColumn {:field "assembly_type" :headerName "Assembly Type" :headerTooltip "Assembly Type" :maxWidth 200 :sortable true :resizable true :filter "agTextColumnFilter"}]
      [:> ag-grid/AgGridColumn {:field "mob_suite_primary_cluster_id" :headerName "MOB-Suite Primary" :maxWidth 200 :sortable true :resizable true :filter "agTextColumnFilter"}]
      [:> ag-grid/AgGridColumn {:field "mob_suite_secondary_cluster_id" :headerName "MOB-Suite Secondary" :maxWidth 200 :sortable true :resizable true :filter "agTextColumnFilter"}]
      [:> ag-grid/AgGridColumn {:field "resistance_gene_name" :headerName "Resistance Gene" :maxWidth 200 :sortable true :resizable true :filter "agTextColumnFilter"}]
      [:> ag-grid/AgGridColumn {:field "resistance_gene_identity" :headerName "Gene Identity (%)" :maxWidth 200 :sortable true :resizable true :filter "agNumberColumnFilter"}]
      [:> ag-grid/AgGridColumn {:field "plasmid_reconstruction_size" :headerName "Plasmid Size" :headerTooltip "Plasmid Reconstruction Size" :maxWidth 200 :sortable true :resizable true :filter "agNumberColumnFilter"}]
      [:> ag-grid/AgGridColumn {:field "num_contigs_in_plasmid_reconstruction" :headerName "Num. Contigs" :headerTooltip "Num. Contigs in Plasmid Reconstruction" :maxWidth 200 :sortable true :resizable true :filter "agNumberColumnFilter"}]
      [:> ag-grid/AgGridColumn {:field "closest_db_plasmid" :headerName "Closest DB Plasmid" :maxWidth 200 :sortable true :resizable true :filter "agTextColumnFilter"}]
      
      
      ]]
    ))



(defn root []
  [:div {:style {:display "grid"
                 :grid-template-columns "1fr"
                 :grid-gap "4px 4px"
                 :height "100%"}} 
   [header]
   [:div {:style {:display "grid"
                  :grid-template-columns "1fr 5fr"
                  :grid-template-rows "repeat(2, 1fr)"
                  :gap "4px"
                  :height "800px"}}
    [:div {:style {:display "grid"
                   :grid-column "1"
                   :grid-row "1 / 3"}}
     [illumina-runs-table]]
    [:div {:style {:display "grid"
                  :grid-column "2"
                  :grid-row "1"
                  :gap "4px"}}
     [library-qc-table]]
    [:div {:style {:display "grid"
                  :grid-column "2"
                  :grid-row "2"
                  :gap "4px"}}
     [plasmid-qc-table]]]
   #_[debug-view]
   ])

(defn main []
  (load-illumina-runs)
  (rdom/render [root] (js/document.getElementById "app")))

(set! (.-onload js/window) main)

(comment

  )
