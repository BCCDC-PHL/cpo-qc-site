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


(def app-version "v0.1.2")


(defn in? 
  "true if coll contains elem"
  [coll elem]  
  (some #(= elem %) coll))


(defn incorporate-run
  "Incorporate a run into a list of runs. If the run is already in the list,
   Add its analysis type to the existing entry."
  [acc run]
  (let [run-id (:run_id run)
        existing-run (get acc run-id)
        run-analysis-type (:analysis_type run)
        run-transformed (-> run
                            (assoc :analysis_types #{run-analysis-type})
                            (dissoc :analysis_type))]
    (if existing-run
      (update-in acc [run-id :analysis_types] conj run-analysis-type)
      (assoc acc run-id run-transformed))))


(defn de-duplicate-sequencing-runs
  "De-duplicate sequencing runs by run id."
  [sequencing-runs]
  (->> sequencing-runs
       (reduce incorporate-run {})
       vals))


(defn load-qc-thresholds
  "Load qc thresholds data and store it in the app db."
  []
  (go
    (let [response (<! (http/get "data/qc_thresholds.json"))]
      (if (= 200 (:status response))
        (swap! db assoc-in [:qc-thresholds] (:body response))))))


(defn load-mlst-scheme-species-name-lookup
  "Load mlst scheme species name lookup data and store it in the app db."
  []
  (go
    (let [response (<! (http/get "data/mlst_scheme_species_name_lookup.json"))]
      (if (= 200 (:status response))
        (swap! db assoc-in [:species-name-by-mlst-scheme] (:body response))))))


(defn load-sequencing-runs
  "Load sequencing runs data and store it in the app db."
  []
  (go
    (let [response (<! (http/get "data/sequencing_runs.json"))]
      (if (= 200 (:status response))
        (swap! db assoc-in [:sequencing-runs] (de-duplicate-sequencing-runs (:body response)))))))


(defn load-library-qc 
  "Load library qc data for a given run id and store it in the app db."
  [run-id]
  (let [analysis-types (:analysis_types (first (filter #(= run-id (:run_id %)) (:sequencing-runs @db))))]
    (doseq [analysis-type analysis-types]
      (go
        (let [response (<! (http/get (str "data/library-qc/" run-id "_" analysis-type "_library_qc.json")))]
          (if (= 200 (:status response))
            (swap! db assoc-in [:library-qc run-id analysis-type] (:body response))))))))


(defn load-plasmid-qc
  "Load plasmid qc data for a given run id and store it in the app db."
  [run-id]
  (let [analysis-types (:analysis_types (first (filter #(= run-id (:run_id %)) (:sequencing-runs @db))))]
    (doseq [analysis-type analysis-types]
      (go
        (let [response (<! (http/get (str "data/plasmid-qc/" run-id "_" analysis-type "_plasmid_qc.json")))]
          (if (= 200 (:status response))
            (swap! db assoc-in [:plasmid-qc run-id analysis-type] (:body response))))))))


(defn get-selected-rows
  "Get selected rows from event object. e is the event object, with type: selectionChanged."
  [e]
  (map #(js->clj (.-data %) :keywordize-keys true)
       (-> e
           .-api
           .getSelectedNodes)))


(defn run-selected
  "Run selected event handler. e is the event object, with type: selectionChanged."
  [e]
  (let [previously-selected-run-ids (:selected-run-ids @db)
        currently-selected-run-ids (map :run_id (get-selected-rows e))
        newly-selected-run-ids (clojure.set/difference (set currently-selected-run-ids) (set previously-selected-run-ids))
        currently-selected-runs (filter #(in? currently-selected-run-ids (:run_id %)) (:sequencing-runs @db))
        newly-selected-runs (filter #(in? newly-selected-run-ids (:run_id %)) (:sequencing-runs @db))]
    (do
      (doall
       (map load-library-qc newly-selected-run-ids))
      (doall
       (map load-plasmid-qc newly-selected-run-ids))
      (swap! db assoc-in [:selected-run-ids] currently-selected-run-ids))))


(defn header
  "Header component."
  []
  [:header {:style {:display "grid"
                    :grid-template-columns "repeat(2, 1fr)"
                    :align-items "center"
                    :height "48px"}}
   [:div {:style {:display "grid"
                  :grid-template-columns "repeat(2, 1fr)"
                  :align-items "center"}}
    [:h1 {:style {:font-family "Arial" :color "#004a87" :margin "0px"}} "CPO QC"][:p {:style {:font-family "Arial" :color "grey" :justify-self "start"}} app-version]]
   [:div {:style {:display "grid" :align-self "center" :justify-self "end"}}
    [:img {:src "images/bccdc_logo.svg" :height "48px"}]]])


(defn sequencing-runs-table
  "Sequencing runs table component."
  []
  (let [runs (:sequencing-runs @db)
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
      [:> ag-grid/AgGridColumn {:field "run_id"
                                :headerName "Run ID"
                                :minWidth 180
                                :resizable true
                                :filter "agTextColumnFilter"
                                :floatingFilter true
                                :sortable true
                                :checkboxSelection true
                                :headerCheckboxSelection true
                                :headerCheckboxSelectionFilteredOnly true
                                :isRowSelectable true
                                :sort "desc"}]]]))


(defn library-qc-table
  "Library QC table component."
  []
  (let [currently-selected-run-ids (:selected-run-ids @db)
        all-library-qc (:library-qc @db)
        selected-runs-library-qc (flatten (map vals (vals (select-keys all-library-qc currently-selected-run-ids))))
        min-megabases-short (get-in @db [:qc-thresholds :min_megabases_short] 50)
        megabases-short-style (fn [params]
                                (if (> min-megabases-short (. params -value))
                                  (clj->js {:backgroundColor "#e6675e"}) nil))
        min-assembly-n50 (get-in @db [:qc-thresholds :min_assembly_n50] 10000)
        assembly-n50-style (fn [params]
                             (if (> min-assembly-n50 (. params -value))
                               (clj->js {:backgroundColor "#e6675e"}) nil))
        species-name-by-mlst-scheme (get @db :species-name-by-mlst-scheme {})
        row-data (->> selected-runs-library-qc
                      (map (fn [x] (assoc x :inferred_species_name (get species-name-by-mlst-scheme (keyword (:mlst_scheme x))))))
                      (map (fn [x] (update x :num_bases_short #(.toFixed (/ % 1000000 ) 2))))
                      (map (fn [x] (update x :num_bases_long #(.toFixed (/ % 1000000 ) 2))))
                      (map (fn [x] (update x :assembly_length #(.toFixed (/ % 1000000 ) 2)))))]
    [:div {:class "ag-theme-balham"
           :style {}}
     [:> ag-grid/AgGridReact
      {:rowData row-data
       :pagination false
       :enableCellTextSelection true
       :onFirstDataRendered #(-> % .-api .sizeColumnsToFit)
       :onSelectionChanged #()}
      [:> ag-grid/AgGridColumn {:field "library_id"
                                :headerName "Library ID"
                                :maxWidth 200
                                :sortable true
                                :resizable true
                                :filter "agTextColumnFilter"
                                :pinned "left"
                                :checkboxSelection false
                                :headerCheckboxSelectionFilteredOnly true
                                :floatingFilter true
                                :sort "desc"}]
      [:> ag-grid/AgGridColumn {:field "assembly_type"
                                :headerName "Assembly Type"
                                :headerTooltip "Assembly Type"
                                :maxWidth 172
                                :sortable true
                                :resizable true
                                :filter "agTextColumnFilter"
                                :floatingFilter true}]
      [:> ag-grid/AgGridColumn {:field "inferred_species_name"
                                :headerName "Inferred Species"
                                :headerTooltip "Inferred Species"
                                :maxWidth 200
                                :sortable true
                                :resizable true
                                :filter "agTextColumnFilter"
                                :floatingFilter true}]
      [:> ag-grid/AgGridColumn {:field "num_bases_short"
                                :type "numericColumn"
                                :headerName "Megabases Short"
                                :headerTooltip "Megabases from Short Reads"
                                :maxWidth 200
                                :sortable true
                                :resizable true
                                :filter "agNumberColumnFilter"
                                :floatingFilter true
                                :cellStyle megabases-short-style}]
      [:> ag-grid/AgGridColumn {:field "num_bases_long"
                                :type "numericColumn"
                                :headerName "Megabases Long"
                                :headerTooltip "Megabases from Long Reads"
                                :maxWidth 200
                                :sortable true
                                :resizable true
                                :filter "agNumberColumnFilter"
                                :floatingFilter true}]    
      [:> ag-grid/AgGridColumn {:field "assembly_length"
                                :type "numericColumn"
                                :headerName "Assembly Length (Mb)"
                                :headerTooltip "Assembly Length"
                                :maxWidth 220
                                :sortable true
                                :resizable true
                                :filter "agNumberColumnFilter"
                                :floatingFilter true}]
      [:> ag-grid/AgGridColumn {:field "assembly_num_contigs"
                                :type "numericColumn"
                                :headerName "Num. Contigs"
                                :headerTooltip "Number of Contigs"
                                :maxWidth 200
                                :sortable true
                                :resizable true
                                :filter "agNumberColumnFilter"
                                :floatingFilter true}]
      [:> ag-grid/AgGridColumn {:field "assembly_N50"
                                :type "numericColumn"
                                :headerName "Assembly N50"
                                :headerTooltip "Assembly N50"
                                :maxWidth 200
                                :sortable true
                                :resizable true
                                :filter "agNumberColumnFilter"
                                :floatingFilter true
                                :cellStyle assembly-n50-style}]
      [:> ag-grid/AgGridColumn {:field "mlst_scheme"
                                :headerName "MLST Scheme"
                                :headerTooltip "MLST Scheme"
                                :maxWidth 200
                                :sortable true
                                :resizable true
                                :filter "agTextColumnFilter"
                                :floatingFilter true}]
      [:> ag-grid/AgGridColumn {:field "mlst_sequence_type"
                                :headerName "MLST Type"
                                :headerTooltip "MLST Sequence Type"
                                :maxWidth 200
                                :sortable true
                                :resizable true
                                :filter "agTextColumnFilter"
                                :floatingFilter true}]]]))


(defn plasmid-qc-table
  "Plasmid QC table component."
  []
  (let [currently-selected-run-ids (:selected-run-ids @db)
        all-plasmid-qc (:plasmid-qc @db)
        selected-runs-plasmid-qc (flatten (map vals (vals (select-keys all-plasmid-qc currently-selected-run-ids))))
        row-data (->> selected-runs-plasmid-qc
                      identity)]
    [:div {:class "ag-theme-balham"
           :style {}}
     [:> ag-grid/AgGridReact
      {:rowData row-data
       :pagination false
       :enableCellTextSelection true
       :onFirstDataRendered #(-> % .-api .sizeColumnsToFit)}
      [:> ag-grid/AgGridColumn {:field "library_id"
                                :headerName "Library ID"
                                :maxWidth 200
                                :sortable true
                                :sort "desc"
                                :resizable true
                                :filter "agTextColumnFilter"
                                :floatingFilter true
                                :pinned "left"
                                :checkboxSelection false
                                :headerCheckboxSelectionFilteredOnly true}]
      [:> ag-grid/AgGridColumn {:field "assembly_type"
                                :headerName "Assembly Type"
                                :headerTooltip "Assembly Type"
                                :maxWidth 172
                                :sortable true
                                :resizable true
                                :filter "agTextColumnFilter"
                                :floatingFilter true}]
      [:> ag-grid/AgGridColumn {:field "mob_suite_primary_cluster_id"
                                :headerName "Primary Plasmid Cluster"
                                :maxWidth 200
                                :sortable true
                                :resizable true
                                :filter "agTextColumnFilter"
                                :floatingFilter true}]
      [:> ag-grid/AgGridColumn {:field "mob_suite_secondary_cluster_id"
                                :headerName "Secondary Plasmid Cluster"
                                :maxWidth 200
                                :sortable true
                                :resizable true
                                :filter "agTextColumnFilter"
                                :floatingFilter true}]
      [:> ag-grid/AgGridColumn {:field "resistance_gene_name"
                                :headerName "Resistance Gene"
                                :maxWidth 200
                                :sortable true
                                :resizable true
                                :filter "agTextColumnFilter"
                                :floatingFilter true}]
      [:> ag-grid/AgGridColumn {:field "resistance_gene_identity"
                                :headerName "Gene Identity (%)"
                                :maxWidth 200
                                :sortable true
                                :resizable true
                                :filter "agNumberColumnFilter"
                                :floatingFilter true}]
      [:> ag-grid/AgGridColumn {:field "plasmid_reconstruction_size"
                                :headerName "Plasmid Size"
                                :headerTooltip "Plasmid Reconstruction Size"
                                :maxWidth 200
                                :sortable true
                                :resizable true
                                :filter "agNumberColumnFilter"
                                :floatingFilter true}]
      [:> ag-grid/AgGridColumn {:field "num_contigs_in_plasmid_reconstruction"
                                :headerName "Num. Contigs"
                                :headerTooltip "Num. Contigs in Plasmid Reconstruction"
                                :maxWidth 200
                                :sortable true
                                :resizable true
                                :filter "agNumberColumnFilter"
                                :floatingFilter true}]
      [:> ag-grid/AgGridColumn {:field "closest_db_plasmid"
                                :headerName "Closest DB Plasmid"
                                :maxWidth 200
                                :sortable true
                                :resizable true
                                :filter "agTextColumnFilter"
                                :floatingFilter true}]
      [:> ag-grid/AgGridColumn {:field "closest_db_plasmid_breadth_coverage_above_depth_threshold"
                                :headerName "Coverage Above 10x (%)"
                                :maxWidth 200
                                :sortable true
                                :resizable true
                                :filter "agNumberColumnFilter"
                                :floatingFilter true}]]]))


(defn root
  "Root component."
  []
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
     [sequencing-runs-table]]
    [:div {:style {:display "grid"
                  :grid-column "2"
                  :grid-row "1"
                  :gap "4px"}}
     [library-qc-table]]
    [:div {:style {:display "grid"
                  :grid-column "2"
                  :grid-row "2"
                  :gap "4px"}}
     [plasmid-qc-table]]]])

(defn render []
  (rdom/render [root] (js/document.getElementById "app")))

(defn re-render []
  (js/console.log "re-rendering")
  (render))

(defn main []
  (load-qc-thresholds)
  (load-mlst-scheme-species-name-lookup)
  (load-sequencing-runs)
  (render))

(defn ^:export init []
  (js/console.log "initializing")
  (set! (.-onload js/window) main))

(comment

  )
