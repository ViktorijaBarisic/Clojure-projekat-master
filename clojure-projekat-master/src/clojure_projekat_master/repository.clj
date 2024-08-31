(ns clojure-projekat-master.repository
  (:require [next.jdbc :as jdbc]
           ))


(def db-url "identifier.sqlite")

(defn get-students-by-age-volunteering-tutoring [age]
  (let [ds (jdbc/get-datasource {:dbtype "sqlite" :dbname db-url})
        query "SELECT * FROM Students WHERE Age = ? AND Volunteering = 1 AND Tutoring = 1 AND Gender = 1"]
    (jdbc/execute! ds [query age])))


(defn average-gpa-by-parental-education []
  (let [ds (jdbc/get-datasource {:dbtype "sqlite" :dbname db-url})
        query "SELECT ParentalEducation, AVG(GPA) AS AvgGPA FROM Students GROUP BY ParentalEducation"]
    (jdbc/execute! ds [query])))


(defn grade-class-label [grade]
  (case (int grade)
    0 "Grade A"
    1 "Grade B"
    2 "Grade C"
    3 "Grade D"
    4 "Grade F"
    "Unknown Grade"))

(defn count-students-by-grade-class []
  (let [ds (jdbc/get-datasource {:dbtype "sqlite" :dbname db-url})
        query "SELECT GradeClass, COUNT(*) AS Count FROM Students GROUP BY GradeClass"
        results (jdbc/execute! ds [query])]
    (map (fn [{:keys [:Students/GradeClass :Count]}]
           {:GradeClass (grade-class-label GradeClass)
            :Count Count})
         results)))


(defn all-data []
  (let [ds (jdbc/get-datasource {:dbtype "sqlite" :dbname db-url})
        query "SELECT * FROM Students"]
    (jdbc/execute! ds [query]))
  )


(defn add-student [db-url student-id age gender ethnicity parental-education study-time-weekly absences tutoring parental-support extracurricular sports music volunteering gpa grade-class]
  (let [ds (jdbc/get-datasource {:dbtype "sqlite" :dbname db-url})
        query "INSERT INTO Students (StudentID, Age, Gender, Ethnicity, ParentalEducation, StudyTimeWeekly, Absences, Tutoring, ParentalSupport, Extracurricular, Sports, Music, Volunteering, GPA, GradeClass) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"]
    (jdbc/execute! ds [query student-id age gender ethnicity parental-education study-time-weekly absences tutoring parental-support extracurricular sports music volunteering gpa grade-class])))


(def db-url "identifier.sqlite")
(def student-id 1002)
(def age 18)
(def gender 1)
(def ethnicity 0)
(def parental-education 3)
(def study-time-weekly 20.5)
(def absences 5)
(def tutoring 0)
(def parental-support 1)
(def extracurricular 1)
(def sports 0)
(def music 0)
(def volunteering 2)
(def gpa 3.4)
(def grade-class 2.0)

;;(add-student db-url student-id age gender ethnicity parental-education study-time-weekly absences tutoring parental-support extracurricular sports music volunteering gpa grade-class)





