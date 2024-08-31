(ns clojure-projekat-master.core
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure-projekat-master.repository :as repository]
            [incanter.core :as incanter]
            [incanter.stats :as stats]
            [incanter.charts :as charts]
            )
  (:import (weka.classifiers.trees J48)
           (weka.core Attribute Instances DenseInstance)
           [weka.classifiers Evaluation])
  (:gen-class)
  )



(def data (repository/all-data))


;; A few firrst rows
(defn rows-to-string [rows]
  (->> rows
       (map #(str %))
       (clojure.string/join "\n")))

(defn print-rows [n rows]
  (let [selected-rows (take n rows)]
    (rows-to-string selected-rows)))



(defn print-attributes [data]
  (let [headers (first data)
        attribute-names (->> headers
                             (map (fn [header]
                                    (clojure.string/replace (name (key header)) "Students/" "")))
                             (remove #(or (= % "GPA") (= % "GradeClass")))
                             (distinct))
        attribute-string (clojure.string/join ", " attribute-names)]
    attribute-string))


;; Checking if there are any missing values
(defn missing? [data]
  (some #(some nil? %) data))

;; Result is nil => no missing values
(defn missing-values []
  (if (missing? data)
    "postoje nedostajuće vrijednosti"
    "nema nedostajućih vrijednosti")
  )

;; Checking if there are any double rows
(defn find-duplicates [data]
  (let [unique-data (set data)]
    (if (= (count data) (count unique-data))
      nil
      (filter #(> (count (filter #{%} data)) 1) data))))

;; Result is nil => no double rows
(defn duplicates []
  (if (find-duplicates data)
    "postoje duplikati"
    "nema duplikata")
  )


;; Preparing data for prediction model

;; Function to split data into training and test sets
(defn split-data [data ratio]
  (let [n (int (* ratio (count data)))]
    [(take n data) (drop n data)]))


;; Splitting data
(def data-split (split-data data 0.8))
(def training-data (first data-split))
(def test-data (second data-split))




(defn create-instances [data]
  (let [grade-class-values ["A" "B" "C" "D" "F"]
        attributes (java.util.ArrayList. [(Attribute. "Age")
                                          (Attribute. "Absences")
                                          (Attribute. "Tutoring")
                                          (Attribute. "Volunteering")
                                          (Attribute. "ParentalEducation")
                                          (Attribute. "StudyTimeWeekly")
                                          (Attribute. "GradeClass" (java.util.ArrayList. grade-class-values))])
        instances (Instances. "StudentPerformance" (java.util.ArrayList. attributes) (count data))
        grade-class-map {0.0 "A" 1.0 "B" 2.0 "C" 3.0 "D" 4.0 "F"}]
    (.setClassIndex instances 6)
    (doseq [row data]
      (let [instance (DenseInstance. (count attributes))]
        (.setDataset instance instances)

        (.setValue instance 0 (double (or (get row :Students/Age) 0)))
        (.setValue instance 1 (double (or (get row :Students/Absences) 0)))
        (.setValue instance 2 (double (or (get row :Students/Tutoring) 0)))
        (.setValue instance 3 (double (or (get row :Students/Volunteering) 0)))
        (.setValue instance 4 (double (or (get row :Students/ParentalEducation) 0)))
        (.setValue instance 5 (double (or (get row :Students/StudyTimeWeekly) 0)))

        ;; Handle GradeClass as a nominal value by setting it as a string directly
        (let [grade-class (or (get row :Students/GradeClass) 0.0)
              nominal-value (get grade-class-map grade-class "A")]
          (.setValue instance (.attribute instances 6) nominal-value))

        (.add instances instance)))
    instances))



;; Prediction using the model
(defn predict-instance [model instance-data dataset]
  (let [attributes-count 6
        instance (DenseInstance. attributes-count)]
    (.setDataset instance dataset)
    (.setValue instance 0 (double (get instance-data 0)))
    (.setValue instance 1 (double (get instance-data 1)))
    (.setValue instance 2 (double (get instance-data 2)))
    (.setValue instance 3 (double (get instance-data 3)))
    (.setValue instance 4 (double (get instance-data 4)))
    (.setValue instance 5 (double (get instance-data 5)))


    ;; Predict using the model
    (.classifyInstance model instance)))


;; Creating instances for training
(def training-instances (create-instances training-data))

;; Training the model
(def model (J48.))
(.buildClassifier model training-instances)




;; model evaluation

(def test-instances (create-instances test-data))

(defn evaluate-model [model test-instances]
  (let [evaluation (Evaluation. test-instances)]
    (.evaluateModel evaluation model test-instances (make-array String 0))

    ;; Ispis rezultata evaluacije
    (println "Summary:")
    (println (.toSummaryString evaluation))

    (println "Confusion Matrix:")
    (doseq [row (.confusionMatrix evaluation)]
      (println (vec row)))

    (println "Class Details:")
    (println (.toClassDetailsString evaluation))

    ;; Vraćamo Evaluation objekat za dalje korišćenje ako je potrebno
    evaluation))


;; Function to prompt the user for input and handle exceptions
(defn prompt-user [prompt validate-fn]
  (loop []
    (println prompt)
    (let [input (try
                  (Integer. (read-line))
                  (catch NumberFormatException _
                    (do
                      (println "Molimo vas unesite validan broj.")
                      nil)))]
      (if (and input (validate-fn input))
        input
        (do
          (println "Unesena vrednost nije u dozvoljenom opsegu, pokušajte ponovo.")
          (recur))))))

(defn get-user-input []
  (let [age (prompt-user "Unesite godine (Age 15-18):" (fn [x] (and (>= x 15) (<= x 18))))
        absence (prompt-user "Unesite broj izostanaka (Absence 0-29):" (fn [x] (and (>= x 0) (<= x 29))))
        tutoring (prompt-user "Da li je učenik imao privatne časove? (0 za ne, 1 za da):" (fn [x] (or (= x 0) (= x 1))))
        volunteering (prompt-user "Da li je učenik volontirao? (0 za ne, 1 za da):" (fn [x] (or (= x 0) (= x 1))))
        parental-education (prompt-user "Unesite nivo obrazovanja roditelja (ParentalEducation 0-4):" (fn [x] (and (>= x 0) (<= x 4))))
        study-time-weekly (prompt-user "Unesite broj sati učenja nedeljno (StudyTimeWeekly 0-20):" (fn [x] (and (>= x 0) (<= x 20))))]
    [age absence tutoring volunteering parental-education study-time-weekly]))



;; Predicting based on user input
(defn get-prediction []
  (let [user-input (get-user-input)
        prediction (predict-instance model user-input training-instances)]
    (case (int prediction)
      0 "Grade A"
      1 "Grade B"
      2 "Grade C"
      3 "Grade D"
      4 "Grade F"
      "Unknown Grade")))


;;How atributes affect output variable
(defn average-grade-by-volunteering [data]
  (let [grouped-by-volunteering (group-by :Students/Volunteering data)
        calculate-average (fn [rows]
                            (let [total (reduce + (map #(get % :Students/GradeClass) rows))
                                  count (count rows)]
                              (/ total count)))]
    (into {} (map (fn [[volunteering rows]]
                    [(if (= volunteering 1) "Volonteri:" "Oni koji ne volontiraju:")
                     (calculate-average rows)])
                  grouped-by-volunteering))))


(defn average-grade-by-tutoring [data]
  (let [grouped-by-tutoring (group-by :Students/Tutoring data)
        calculate-average (fn [rows]
                            (let [total (reduce + (map #(get % :Students/GradeClass) rows))
                                  count (count rows)]
                              (/ total count)))]
    (into {} (map (fn [[tutoring rows]]
                    [(if (= tutoring 1) "Imaju mentora" "Nemaju mentora")
                     (calculate-average rows)])
                  grouped-by-tutoring))))




(defn average-grade-by-absences [data]
  (let [grouped-by-absence-range (group-by (fn [row]
                                             (let [absences (get row :Students/Absences)]
                                               (cond
                                                 (<= absences 4) "0-4"
                                                 (<= absences 8) "5-8"
                                                 (<= absences 12) "9-12"
                                                 (<= absences 16) "13-16"
                                                 (<= absences 20) "17-20"
                                                 (<= absences 23) "21-23"
                                                 :else "24-29"))) data)
        calculate-average (fn [rows]
                            (let [total (reduce + (map #(get % :Students/GradeClass) rows))
                                  count (count rows)]
                              (/ total count)))
        average-map (into {} (map (fn [[range rows]]
                                    [range (calculate-average rows)])
                                  grouped-by-absence-range))]
    (let [key-order ["0-4" "5-8" "9-12" "13-16" "17-20" "21-23" "24-29"]]
      (into {} (map (fn [k] [k (get average-map k)])
                    key-order)))))



(defn average-grade-by-parental-education [data]
  (let [grouped-by-parental-education (group-by :Students/ParentalEducation data)
        calculate-average (fn [rows]
                            (let [total (reduce + (map #(get % :Students/GradeClass) rows))
                                  count (count rows)]
                              (/ total count)))
        average-map (into {} (map (fn [[education-level rows]]
                                    [education-level (calculate-average rows)])
                                  grouped-by-parental-education))]
    (into {} (sort-by key average-map))))



(defn average-grade-by-study-time [data]
  (let [grouped-by-study-time (group-by (fn [row]
                                          (let [study-time (get row :Students/StudyTimeWeekly)]
                                            (cond
                                              (<= study-time 5) "0-5"
                                              (<= study-time 10) "6-10"
                                              (<= study-time 15) "11-15"
                                              :else "16+"))) data)
        calculate-average (fn [rows]
                            (let [total (reduce + (map #(get % :Students/GradeClass) rows))
                                  count (count rows)]
                              (/ total count)))
        average-map (into {} (map (fn [[study-time-range rows]]
                                    [study-time-range (calculate-average rows)])
                                  grouped-by-study-time))]
    (let [key-order ["0-5" "6-10" "11-15" "16+"]]
      (into {} (map (fn [k] [k (get average-map k)])
                    key-order)))))


;;does not show any trends
(defn study-time-vs-absences [data]
  (let [grouped-by-absences (group-by (fn [row]
                                        (let [absences (get row :Students/Absences)]
                                          (cond
                                            (<= absences 4) "0-4"
                                            (<= absences 8) "5-8"
                                            (<= absences 12) "9-12"
                                            (<= absences 16) "13-16"
                                            (<= absences 20) "17-20"
                                            (<= absences 23) "21-23"
                                            :else "24-29"))) data)
        calculate-average-study-time (fn [rows]
                                       (let [total (reduce + (map #(get % :Students/StudyTimeWeekly) rows))
                                             count (count rows)]
                                         (/ total count)))]
    (into {} (map (fn [[absence-range rows]]
                    [absence-range (calculate-average-study-time rows)])
                  grouped-by-absences))))




;;correlations between atributes and output variable
(defn correlation-tutoring-gradeclass [data]
  (let [tutoring (map :Students/Tutoring data)
        grades (map :Students/GradeClass data)]
    (stats/correlation tutoring grades)))


(defn correlation-volunteering-gradeclass [data]
  (let [volunteering (map :Students/Volunteering data)
        grades (map :Students/GradeClass data)]
    (stats/correlation volunteering grades)))


(defn correlation-parental-education-gradeclass [data]
  (let [parentalEducation (map :Students/ParentalEducation data)
        grades (map :Students/GradeClass data)]
    (stats/correlation parentalEducation grades)))


(defn correlation-absences-gradeclass [data]
  (let [absences (map :Students/Absences data)
        grades (map :Students/GradeClass data)]
    (stats/correlation absences grades)))

(correlation-tutoring-gradeclass data)
(correlation-volunteering-gradeclass data)
(correlation-parental-education-gradeclass data)
(correlation-absences-gradeclass data)


;;plots
(defn plot-absences-vs-gradeclass [data]
  (let [absences (map :Students/Absences data)
        grades (map :Students/GradeClass data)
        grouped-data (group-by identity absences)
        binned-data (map (fn [[bin values]]
                           {:Absences bin
                            :AverageGrade (double (/ (apply + values)
                                                     (count values)))})
                         (sort-by key (into {} (map (fn [[k v]]
                                                      [k (map (fn [d] (:Students/GradeClass d))
                                                              (filter #(= (:Students/Absences %) k) data))])
                                                    grouped-data))))
        absences (map :Absences binned-data)
        avg-grades (map :AverageGrade binned-data)
        chart (charts/bar-chart absences avg-grades
                                :x-label "Absences"
                                :y-label "Average Grade Class"
                                :title "Average Grade Class by Absences")]
    (incanter/view chart)))


(defn plot-absences-studytime-gradeclass0 [data]
  (let [absences (map :Students/Absences data)
        study-time (map :Students/StudyTimeWeekly data)
        grades (map :Students/GradeClass data)
        unique-grades (distinct grades)
        colors (map (fn [grade]
                      (case grade
                        1 "blue"
                        2 "red"
                        3 "green"
                        4 "orange"
                        0 "black"))
                    grades)
        chart (charts/scatter-plot absences study-time
                                   :x-label "Absences"
                                   :y-label "Study Time Weekly"
                                   :title "Absences vs Study Time Weekly"
                                   :group-by grades
                                   :color colors)]
    (incanter/view chart)))


(defn plot-absences-studytime-gradeclass [data]
  (let [absences (map :Students/Absences data)
        study-time (map :Students/StudyTimeWeekly data)
        grades (map :Students/GradeClass data)
        colors (map (fn [grade]
                      (case grade
                        0 "purple"
                        1 "blue"
                        2 "red"
                        3 "green"
                        4 "orange"
                        "black"
                        ;; Default case for undefined grades
                        )) ;; Use purple for any unexpected values
                    grades)
        chart (charts/scatter-plot absences study-time
                                   :x-label "Absences"
                                   :y-label "Study Time Weekly"
                                   :title "Absences vs Study Time Weekly"
                                   :group-by grades
                                   :color colors)]
    (incanter/view chart)))



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Zdravo, korisniče, pred tobom se nalazi program koji obavlja analizu podataka vezanih za uspjeh učenika.")
  (println "..........................................................")
  (println "Podaci su učitani i ovo je ispis prvih nekoliko redova:")
  (println (print-rows 3 data))
  (println "..........................................................")
  (println "Dakle, atributi koje sadrži ovaj data set su sljedeći:")
  (println (print-attributes data))
  (println "..........................................................")
  (println "Sada ćemo vidjeti više o strukturi data seta")
  (println "Ispisujemo učenice koje imaju 15 godina, imaju mentora i volontiraju:")
  (println (repository/get-students-by-age-volunteering-tutoring 15))
  (println "..........................................................")
  (println "Hajde da prebrojimo koliko ima učenika sa kojom ocjenom u data setu:")
  (println (repository/count-students-by-grade-class))
  (println "..........................................................")
  (println "Hajde da vidimo prosječnu ocjenu učenika u zavisnosti od stepena obrazovanja njihovih roditelja:")
  (repository/average-gpa-by-parental-education)
  (println "..........................................................")
  (println "Sada ćemo vidjeti da li postoje neke vrijednosti koje nedostaju u data setu:" (missing-values))
  (println "..........................................................")
  (println "Sada ćemo vidjeti da li postoje neke duplirane vrijednosti u data setu:" (duplicates))
  (println "..........................................................")
  (println "Sada, kada su podaci spremni za rad, možemo da krenemo sa predvidjanjem")
  (println "..........................................................")
  (println "Sada možeš i sam da uneseš određene vrijednosti, a zatim ćeš dobiti predviđanje koja je očekivana ocjena za tog učenika. Obrati pažnju na raspon dozvoljenih vrijdnosti za unos.")
  (println "..........................................................")
  (println "Student ce imati ocenu:" (get-prediction))
  (println "..........................................................")
  (println "Sada, kada si dobio očekivanu ocjenu za unesenog učenika, hajde da vidimo kako se razlikuju prosječne ocjene za različite vrijednosti pomenutih atributa")
  (println (average-grade-by-volunteering data))
  (println (average-grade-by-tutoring data))
  (println "U zavisnosti od broja izostanaka na sedmičnom nivou:")
  (println (average-grade-by-absences data))
  (println "U zavisnosti od nivoa obrazovanja roditelja:")
  (println (average-grade-by-parental-education data))
  (println "U zavisnosti od sati učenja sedmično:")
  (println (average-grade-by-study-time data))
  (println "Dakle, učenici koji uče najviše, neće uvijek imati najbolju ocjenu :)")
  (println "Work smart, not hard je dokazano i ovime, jer učenici koji uče 15 sati sedmično imaju bolje ocjene od učenika koji uče 20 sati")
  (println "..........................................................")
  (println "Da li te zanima koji to od faktor najviše utiče na to koju će ocjenu učenik imati?")
  (println "U nastavku možeš vidjeti koliki uticaj ima koji od prethodno pomenutih atributa.")
  (println "..........................................................")
  (println "Mentorstvo:" (correlation-tutoring-gradeclass data))
  (println "Volontiranje:" (correlation-volunteering-gradeclass data))
  (println "Obrazovanje roditelja:" (correlation-parental-education-gradeclass data))
  (println "Odsustvo:" (correlation-absences-gradeclass data))
  (println "Dakle, najveći uticaj ima odsustvo. To da li učenik ima mentora, da li su mu roditelji obrazovani i da li volontira ima dosta mali uticaj na to koju ocjenu će dobiti")
  (println "..........................................................")
  (println "Hvala ti na učešću! Pozdrav!")
  (println "..........................................................")

  )










