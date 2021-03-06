

# 研究所期間完成之論文研究與資料分析專案

# 論文
**論文題目** : 應用Conway-Maxwell-Poisson分配預測非契約型顧客之終身價值   

**[摘要]**   
隨著商業競爭加劇，企業不再單純依靠產品本身差異以維持競爭力，進而將焦點轉向個人化之服務，然而在顧客數眾多的情況下，如何評估個別顧客為企業帶來的終身價值 (customer lifetime value, 簡稱CLV或LTV) 已儼然成為重要的課題。若企業可明確知道顧客流失時點則稱為契約型關係 (contractual relations)，反之則稱為非契約型關係 (non-contractual relations)。本論文探討的是非契約型關係，考慮顧客在企業中存續時間為不可觀測之下，分別建構交易次數與顧客存續時間模型及交易金額模型之後，再依據CLV的計算公式，以預測個別顧客的CLV。不少實證研究顯示，交易次數相較於卜瓦松分配有過度離散 (overdispersion) 或不足離散 (underdispersion) 的現象，本論文乃延續 Mzoughia et al. (2018) 的做法，以Conway-Maxwell-Poisson (CMP) 分配為交易次數之分配，但修正Mzoughia et al. (2018) 的公式，納入顧客間交易次數離散現象之異質性，並進一步推導及計算出兩種CLV估計值，可分別評估顧客未來於一定期間內及至其流失為止的價值。   
   
**[使用方法]**   
- 程式 : R
- 研究方法 : CLV估計模型建構、MCMC Metropolis-Hastings、資料視覺化   
   
**[程式碼]**   
- 檔案名稱 : Thesis_CLVModel.R
- 檔案連結 : https://github.com/ElinorChens/SchoolProjects/blob/master/Thesis_CLVModel.R

# 資料分析專案
## 1. 台北市Airbnb房價預測資料分析   
**[專案概要]**   
  利用Airbnb平台中台北市出租房間資料，分析方法應用了探索性分析、PCA主成份分析、Correspondence Analysis、K-means、Random Forest、XGBoost，整合這許多方法之分析結果後，最終提出分別給予Airbnb平台方、房東具體之建議。   

**[使用方法]**   
- 程式 : R
- 分析方法 : 探索性分析、PCA主成份分析、Correspondence Analysis、Factor Analysis、K-means、Random Forest、XGBoost

**[程式碼]**   
- 檔案名稱 : AirbnbTpe.Rmd
- 檔案連結 : https://github.com/ElinorChens/SchoolProjects/blob/master/AirbnbTpe.Rmd

## 2. 解密都市傳說 : 教授以風扇吹散報告遠近決定學生成績之實驗與影響成績之因素探討
**[專案概要]**   
  實際進行實驗，模擬教授以風扇吹散報告之情況。以風扇風速、紙張厚度、繳交順序為主要因子，並控制其他變數相同下，收集12種條件組合的實驗數據，分析結果發現唯有紙張厚度對於飛散距離具有顯著影響，使用紙張厚度較厚者，飛散距離顯著較使用厚度薄者近，表示在本實驗情境下，使用紙張厚度較厚者較可能獲得較高成績。   
   
**[使用方法]**   
- 程式 : R
- 使用方法 : 三因子實驗設計、ANOVA 分析   
   
**[程式碼]**   
- 檔案名稱 : FanScoringExperiment.R
- 檔案連結 : https://github.com/ElinorChens/SchoolProjects/blob/master/FanScoringExperiment.R

## 3. 鐵達尼號沈船事件乘客與船員生還資料分析
**[專案概要]**   
使用鐵達尼號乘客與船員資料，分析可能影響生還的因素與建立預測模型。
   
**[使用方法]**   
- 程式 : R
- 使用方法 : 探索性資料分析、Logistic Regression、Random Forest   
   
**[程式碼]**   
- 檔案名稱 : Titanic.R
- 檔案連結 : https://github.com/ElinorChens/SchoolProjects/blob/master/Titanic.R

## 4. 某零售商店顧客終身價值(CLV)之影響因素探討與預測模型建立   
**[專案概要]**   
利用 kaggle 網站上某實體零售商之交易資料，以顧客終身價值 (Customer Lifetime Value, CLV) 做為預測應變數進行分析，探討影響因素與建立CLV預測模型。   
   
**[使用方法]**   
- 程式 : R
- 使用方法 : 探索性資料分析、Linear Regression、Random Forest

**[程式碼]**   
- 檔案名稱 : RetailCLVModel.R
- 檔案連結 : https://github.com/ElinorChens/SchoolProjects/blob/master/RetailCLVModel.R

## 5. 台南市民眾投保颱風洪水險意願與影響變數探討   
**[專案概要]**   
資料取自學術調查研究資料庫(SRDA)其中研究計劃所收集之問卷資料，使用其中民眾針對颱風洪水險意願有關的變數資料進行分析，透過建立邏輯斯迴歸模型了解影響民眾投保颱風洪水險意願之原因，分析結果發現保費價格、財產損失認知嚴重程度、教育程度與投保意願具有顯著影響，而是否有防洪設施與收入則不顯著。   
   
**[使用方法]**   
- 程式 : R
- 使用方法 : 探索性資料分析、Logistic Regression   
   

**[程式碼]**   
- 檔案名稱 : FloodInsurance.Rmd
- 檔案連結 : https://github.com/ElinorChens/SchoolProjects/blob/master/FloodInsurance.Rmd
