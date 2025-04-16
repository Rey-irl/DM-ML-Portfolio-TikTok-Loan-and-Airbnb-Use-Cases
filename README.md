# 📊 Applied Data Mining & Machine Learning Portfolio

This project is a hands-on portfolio that demonstrates the application of **Data Mining (DM)** and **Machine Learning (ML)** algorithms on real-world datasets. It showcases the end-to-end process of transforming raw data into predictive models and valuable insights using the **CRISP-DM methodology**.

👩‍💻 **Author**: Reyna Vargas Antonio  
🎓 National College of Ireland | School of Computing  
📧 x23127635@student.ncirl.ie

---

## 🧭 Methodology: CRISP-DM

1. **Business Understanding**
2. **Data Understanding**
3. **Data Preparation**
4. **Modeling**
5. **Evaluation**
6. **Deployment (simulated)**

---

## 📦 Projects Overview

### 1️⃣ **TikTok Dataset (Classification)**  
**Goals:**
- Predict if video reviews are *complaints* or *opinions*
- Analyze text reviews using **Logistic Regression** and **Naïve Bayes**

#### 🔍 Techniques:
- Logistic Regression on metadata (e.g., likes, views, shares)
- Text classification using Naïve Bayes and word frequency analysis
- Model evaluation via **ROC Curve**, **Confusion Matrix**, and **Accuracy**

#### 🧠 Key Results:
- **Logistic Regression Accuracy:** 72.72%
- **Naïve Bayes Text Classifier:** 96% opinions detected
- Most complaint-indicative words: “stop,” “scam,” “refund”

---

### 2️⃣ **Loan Dataset (Regression + Classification)**  
**Goals:**
- Predict loan amounts using **Multiple Linear Regression**
- Classify loan status and term using **K-Nearest Neighbors (K-NN)**

#### 🔍 Techniques:
- Regression to model `loan_amount` from numeric/categorical data
- K-NN classification for `verification_status` and `term`
- Outlier handling, feature transformation (e.g., log scale), and VIF analysis

#### 🧠 Key Results:
- **MLR R² Accuracy:** 86.78%
- **K-NN Model 1 Accuracy:** Rejected (Kappa < 0.2)
- **K-NN Model 2 Accuracy:** 93.09% (Kappa = 0.79)
- Influential factors for 36/60 month terms: `int_rate`, `loan_amount`, `total_payment`

---

### 3️⃣ **Airbnb Dataset (Decision Tree)**  
**Goals:**
- Predict **room type** bookings based on price, reviews, and availability

#### 🔍 Techniques:
- Decision Tree Classifier with features like `price`, `minimum_nights`, `reviews`, etc.
- Preprocessing includes handling outliers, normalizing, and removing non-informative features

#### 🧠 Key Results:
- **Accuracy:** 89.22%
- **Kappa:** 0.6366
- High likelihood of booking **entire homes/apartments** at lower prices
- Hotel rooms and shared rooms were rarely selected

---

## 🧰 Tools & Libraries

- **Languages:** R (primary)
- **Libraries:** `caret`, `tm`, `e1071`, `rpart`, `ROCR`, `ggplot2`
- **Platform:** Kaggle Datasets

---

## 🔮 Future Work

- Apply **ensemble methods** (Random Forest, XGBoost)
- Automate preprocessing pipeline
- Integrate dashboards for dynamic result interpretation

---

## 📚 References

- Hosmer et al. (2013). *Applied Logistic Regression*
- Pardoe (2020). *Applied Regression Modeling*
- Miner et al. (2012). *Practical Text Mining*
- Nguyen et al. (2023). *Statistical Models in Python*
- Delen (2014). *Real-World Data Mining*
- Kaggle Datasets: [TikTok](https://www.kaggle.com/datasets/yakhyojon/tiktok), [Loan](https://www.kaggle.com/datasets/nezukokamaado/auto-loan-dataset), [Airbnb](https://www.kaggle.com/datasets/deeplearner09/airbnb-listings)

---


