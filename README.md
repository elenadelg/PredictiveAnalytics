# Predictive Analytics: Time Series Analysis of Google Searches of "Pasta" in Italy

## 📚 Introduction
This analysis explores the evolving patterns in Google search trends for "pasta" in Italy, aiming to identify significant shifts in public interest. By employing time series forecasting techniques, such as **Exponential Smoothing (ETS)** and **AutoRegressive Integrated Moving Average (ARIMA)** models, the study helps anticipate future interest in pasta. Businesses can leverage these insights to adjust their marketing strategies accordingly.

The analysis involves a comprehensive exploration of temporal patterns, structural breaks, and model performance to identify the optimal forecasting configuration. The results highlight that the **ARIMA model** outperforms the ETS model, particularly in capturing complex temporal dynamics.

---

## 📊 Data Collection and Description
The dataset, sourced from **Google Trends**, includes **246 observations** spanning from **2004 to the present day**, tracking the relative frequency of searches for "pasta" in Italy. This normalized dataset ensures that fluctuations in search volume are not biased by the overall increase in Google search traffic over time (Choi & Varian, 2012).

### Key Characteristics:
- **Data Source:** Google Trends
- **Time Period:** 2004 to present
- **Normalization:** Adjusted to account for growth in search traffic

---

## 🛠️ Methodology

### 1. **Data Preparation**
- **Trend, Seasonality, and Autocorrelation:**  
  The **Autocorrelation Function (ACF)** and **Partial Autocorrelation Function (PACF)** were examined to identify temporal dependencies.
  
- **Box-Cox Transformation:**  
  Addressed strong autocorrelations using the **Box-Cox transformation**, with Guerrero's method for optimal lambda selection.

- **STL Decomposition:**  
  Decomposed the time series into **trend**, **seasonal**, and **remainder** components using **Seasonal and Trend decomposition using Loess (STL)**.

- **Stationarity Check:**  
  Performed stationarity tests using **Augmented Dickey-Fuller (ADF)** and **Kwiatkowski–Phillips–Schmidt–Shin (KPSS)** tests.

- **Differencing:**  
  Applied differencing to stabilize the time series by removing trends and seasonality.

- **Structural Break Detection:**  
  The **Quandt-Likelihood Ratio (QLR)** test detected structural breaks, enabling segmentation of the data for better forecasting.

---

### 2. **Model Evaluation and Forecasting**
- **Dataset Split:**  
  Split the dataset with approximately **70% for training** and **30% for testing**.

- **Evaluation Metrics:**  
  - **Information Criteria:** Used **AIC**, **AICc**, and **BIC** to select optimal models.
  - **Residual Diagnostics:** Analyzed autocorrelation and heteroskedasticity in residuals.
  - **Accuracy Metrics:** Evaluated test set performance using **RMSE (Root Mean Square Error)** and **MASE (Mean Absolute Scaled Error)**.

---

## 📈 Models

### 1. **ARIMA Model**
- **Components:** Auto-Regressive (AR), Differencing (I), and Moving Average (MA).
- **Purpose:** Capture autocorrelations, trends, and seasonality by differencing and modeling past errors.
- **Optimal Configuration:** ARIMA(2,1,2)(1,1,1)

### 2. **ETS Model**
- **Components:** Error (E), Trend (T), and Seasonality (S) components (additive or multiplicative).
- **Purpose:** Apply exponentially decreasing weights to past observations for time series smoothing.
- **Optimal Configuration:** ETS(A,N,A)

---

## 🔍 Conclusion
The study found that the **ARIMA(2,1,2)(1,1,1)** model outperformed the **ETS(A,N,A)** model due to its superior capability in capturing complex temporal dynamics. Key takeaways include:

- **ARIMA vs. ETS:**  
  The ARIMA model demonstrated better residual diagnostics and lower out-of-sample errors.

- **ETS Limitations:**  
  Although ETS performed competitively on the test set, residual diagnostics showed significant autocorrelation and heteroskedasticity, reducing its predictive reliability.

- **Future Research:**  
  ARIMA models lack predictive variables that could further improve accuracy. Future studies could incorporate additional variables, such as economic indicators or seasonal marketing events.


---

## 📢 References
- Choi, H., & Varian, H. (2012). Predicting the Present with Google Trends.
- Google Trends. https://trends.google.com
  
---

## 📂 Repository Structure
```plaintext
/PastaSearch_TimeSeriesAnalysis
    ├── README.md
    ├── PredictiveAnalytics_files/ # storing figures
        └── figure-gfm/ 
            ├── plot1.png
            ├── plot2.png
            └── ...
    └── PredictiveAnalytics.md  
