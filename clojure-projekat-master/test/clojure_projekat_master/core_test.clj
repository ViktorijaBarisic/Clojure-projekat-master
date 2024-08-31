(ns clojure-projekat-master.core-test
  (:require [clojure.test :refer :all]
            [midje.sweet :refer :all]
            [clojure-projekat-master.core :refer :all]))



(fact "Testiranje funkcije koja vraća da li postoje nedostajuće vrijednosti"
      (with-redefs [missing? (fn [_] true)]
        (missing-values) => "postoje nedostajuće vrijednosti")

      (with-redefs [missing? (fn [_] false)]
        (missing-values) => "nema nedostajućih vrijednosti"))

(facts "Predikcija na osnovu unosa studenta"
       (fact "Predikcija ocjene A"
             (against-background
               (get-user-input) => {:Students/Hours 10 :Students/Absences 2}
               (predict-instance model {:Students/Hours 10 :Students/Absences 2} training-instances) => 0)
             (get-prediction) => "Grade A")

       (fact "Predikcija ocjene C"
             (against-background
               (get-user-input) => {:Students/Hours 5 :Students/Absences 5}
               (predict-instance model {:Students/Hours 5 :Students/Absences 5} training-instances) => 2)
             (get-prediction) => "Grade C")

       (fact "Predikcija ocjene F"
             (against-background
               (get-user-input) => {:Students/Hours 1 :Students/Absences 10}
               (predict-instance model {:Students/Hours 1 :Students/Absences 10} training-instances) => 4)
             (get-prediction) => "Grade F"))


(facts "Prosečne ocjene u zavisnosti od volontiranja"
       (fact "Volonteri"
             (average-grade-by-volunteering [{:Students/Volunteering 1 :Students/GradeClass 2}
                                             {:Students/Volunteering 1 :Students/GradeClass 3}])
             => {"Volonteri:" 5/2})

       (fact "Prosečna ocena za one koji ne volontiraju"
             (average-grade-by-volunteering [{:Students/Volunteering 0 :Students/GradeClass 4}
                                             {:Students/Volunteering 0 :Students/GradeClass 4}
                                             {:Students/Volunteering 0 :Students/GradeClass 3}])
             => {"Oni koji ne volontiraju:" 11/3}))



(fact "Korelacija između atributa mentrostvo i ocjene"
      (correlation-tutoring-gradeclass [{:Students/Tutoring 2 :Students/GradeClass 3}
                                        {:Students/Tutoring 5 :Students/GradeClass 4}
                                        {:Students/Tutoring 1 :Students/GradeClass 2}])
      => (roughly 0.96 0.01))





