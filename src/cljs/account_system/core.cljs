(ns account-system.core
  (:require
   [reagent.core :as reagent :refer [atom]]
   [reagent.dom :as rdom]
   [reagent.session :as session]
   [clojure.string :as str]
   [cljsjs.react-vis :as rvis]
   [cljs-time.core :as time]
   [cljs-time.format :as time-fmt]
   [reitit.frontend :as reitit]
   [clerk.core :as clerk]
   [accountant.core :as accountant]))

;;;; State
(def transactions (atom [{:date (time/date-time 2021 2 17 12 45)
                          :receiver "John Walker" :amount 300}
                         {:date (time/date-time 2021 3 12 23 20)
                          :receiver "David Cruger" :amount 2850}
                         {:date (time/date-time 2021 4 8 8 10)
                          :receiver "Mary Silver" :amount 1380}
                         {:date (time/date-time 2021 4 10 03 30)
                          :receiver "Tamzin Mendoza" :amount 3275}
                         {:date (time/date-time 2021 6 17 21 30)
                          :receiver "Bob Morris" :amount 4530}
                         {:date (time/date-time 2021 5 23 14 53)
                          :receiver "Henry Ford" :amount 6580}
                         {:date (time/date-time 2021 7 10 13 00)
                          :receiver "Michael Collins" :amount 2062}
                         {:date (time/date-time 2021 8 19 20 30)
                          :receiver "Lucy Harper" :amount 2834}
                         {:date (time/date-time 2021 10 11 11 20)
                          :receiver "Mike O'Brien" :amount 2500}]))

(def currency "UAH")
(def account-cash (atom 5000))

;;;; Transactions Logic
(defn create-transaction
  "Validate data and return transaction or nil."
  [receiver amount]
  ;; FIXME move validation
  (let [invalid-amount? (or (js/isNaN (js/parseInt @amount))
                            (< @account-cash @amount))
        invalid-receiver? (str/blank? (str/trim @receiver))]
    (when invalid-receiver?
      (js/alert "Please fill in the receiver field!"))
    (when invalid-amount?
      (js/alert "Invalid amount!"))
    (if (or invalid-amount? invalid-receiver?)
      nil
      {:date (time/time-now)
       :receiver @receiver
       :amount (js/Math.round @amount)})))

(defn process-transaction!
  "Update `account-cash` and `transactions`."
  [transaction]
  (swap! account-cash - (:amount transaction))
  (swap! transactions conj transaction)
  ;; REVIEW Sort transactions by date just in case
  (let [sorted-transactions
        (sort-by #(:date %) time/before? @transactions)]
    (reset! transactions sorted-transactions)))

;;;; Utils
(def months ["January" "February" "March"
             "April" "May" "June"
             "July" "August" "September"
             "October" "November" "December"])

(defn format-time-for-table
  "Get date in MySQL style."
  [date]
  (time-fmt/unparse (time-fmt/formatters :mysql) date))

(defn get-transaction-month [{:keys [date]}]
  (nth months (dec (time/month date))))

(defn get-current-month []
  (nth months (dec (time/month (time/time-now)))))


;;;;; Page components

;;;; Charts

(defn app-scaffold
  "Wrappper for charts."
  [chart chart-data]
  [:div {:align "center"}
   [chart chart-data]])

;;; By-month chart
(defn expenses-by-month
  "Get map with all months and amount of money spent in it e.g. {April 6968, May 3583, July 2062, August 2834, October 2500}."
  [transactions]
  (apply merge-with + (for [transaction @transactions]
                      {(get-transaction-month transaction)
                         (:amount transaction)})))

(defn by-month-chart-data-map
  "Get data for by-month chart e.g. {:x April :y 6968}."
  [transactions]
  (vec (map (fn [[month money]] {:x month :y money})
            (expenses-by-month transactions))))

;;; Atom to store chart-data state
(def by-months-chart-data (atom (by-month-chart-data-map transactions)))

(defn by-month-amount-chart [data]
  [:> rvis/FlexibleXYPlot
   {:id "by-month-chart"
    :height 600
    :xType "ordinal"
    :animation true}
   [:> rvis/XAxis]
   [:> rvis/YAxis  {:left 15 
                    :style {:font-size 10}}]
   [:> rvis/VerticalBarSeries
    {:data @data
     :color "#D9E3DA"}]])

;;; Gauge chart
(defn get-expense-ratio
  "Get the ratio of money spent to money left."
  [transactions account-cash]
  (let [expenses (expenses-by-month transactions)
        current-month (get-current-month)
        current-month-amount (second (find expenses current-month))]
    (/ (+ current-month-amount @account-cash)
       current-month-amount)))

(defn gauge-chart-data-map
  [expense-ratio]
  [{:angle0 1.5  :angle (* 4 (/ js/Math.PI 4)) :radius 0, :radius0 0}
   {:angle0 (/ js/Math.PI expense-ratio) :angle 0, :radius 1.1, :radius0 0.8}])

;;; Atom to store chart-data state
(def gauge-chart-data (atom (gauge-chart-data-map (get-expense-ratio transactions account-cash))))

(defn gauge-chart [data]
  (fn []
    (let [expense-ratio (get-expense-ratio transactions account-cash)
          green-color "#6CEE5E"
          yellow-color "#FFFF66"
          red-color "#FF3333"]
      [:> rvis/XYPlot
       {:id "by-month-chart"
        :width 800 :height 400
        :animation true
        :xDomain [0, 10]
        :yDomain [0, 10]
        :style {:transform "rotate(270deg)"}}
       [:> rvis/ArcSeries
        {:data @data
         :animation true
         :color (cond
                  (<= 1 expense-ratio 1.5) red-color
                  (<= 1.5 expense-ratio 2) yellow-color
                  :else green-color)
         :center {:x 5 :y 5}}]])))

(defn home-page []
  (fn []
    [:span.main
     [:h1 {:align "center"} "Accounting system"]]))

(defn transactions-table [transactions]
  [:table {:id "transactions"
           :align "center"
           :style {:text-align "center"
                   :width "100%"}}
   [:thead
    [:tr
     [:th "Date"]
     [:th "Receiver"]
     [:th "Amount"]]]
   [:tbody
    (for [transaction (reverse @transactions)]
      (let [{:keys [date receiver amount]} transaction]
        [:tr
         [:td (format-time-for-table date)]
         [:td receiver]
         [:td (str amount " " currency)]]))]])

(defn transaction-input-forms []
  (let [receiver (atom "")
        amount (atom nil)]
    (fn []
      [:form {:id "transaction-form"
              :align "center"
              :style {:text-align "center"}
              :on-submit (fn [event]
                           (.preventDefault event)
                           ;; If transaction is valid, add it to transactions "database" and update charts
                           (when-let [transaction (create-transaction receiver amount)]
                             (process-transaction! transaction)
                             (reset! by-months-chart-data (by-month-chart-data-map transactions))
                             (reset! gauge-chart-data (gauge-chart-data-map (get-expense-ratio transactions account-cash)))
                             ;; Clear input fields
                             (reset! receiver "")
                             (reset! amount nil)))}
       [:input {:id "receifer-form"
                :type "text"
                :placeholder "Receiver"
                :value @receiver
                :on-change #(reset! receiver (.-value (.-target %)))}]
       [:input {:id "amount-form"
                :type "text"
                :value @amount
                :placeholder "Amount"
                :on-change #(reset! amount (.-value (.-target %)))}]
       [:input {:type "submit" :value "Submit"}]])))

;; Routes
(def router
  (reitit/router
   [["/" :index]]))

;; Translate routes -> page components
(defn page-for [route]
  (case route
    :index #'home-page))

;; Page mounting component

(defn current-page []
  (fn []
    (let [page (:current-page (session/get :route))]
      [:div
       [:header]
       [page]
       [:h4 {:style {:text-align "center"}}
        (str "Amount of money: " @account-cash " " currency)]
       [transaction-input-forms]
       [:h4 {:style {:text-align "center"
                     :margin-top "50px"}} "Transactions:"]
       [transactions-table transactions]
       [:h4 {:style {:text-align "center"}} "Expenses by month:"]
       [app-scaffold by-month-amount-chart by-months-chart-data]
       [:h4 {:style {:text-align "center"}} "Current month expenses:"]
       [:p (str "Spent this month: " (-> (expenses-by-month transactions)
                                         (find (get-current-month))
                                         (second)) " " currency)]
       [:p (str "Left: " @account-cash " " currency)]
       [app-scaffold gauge-chart gauge-chart-data]])))


;; Initialize app

(defn mount-root []
  (rdom/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (clerk/initialize!)
  (accountant/configure-navigation!
   {:nav-handler
    (fn [path]
      (let [match (reitit/match-by-path router path)
            current-page (:name (:data  match))
            route-params (:path-params match)]
        (reagent/after-render clerk/after-render!)
        (session/put! :route {:current-page (page-for current-page)
                              :route-params route-params})
        (clerk/navigate-page! path)))
    :path-exists?
    (fn [path]
      (boolean (reitit/match-by-path router path)))})
  (accountant/dispatch-current!)
  (mount-root))
