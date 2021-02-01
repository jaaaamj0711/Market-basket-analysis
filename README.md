# :cat: :dog: 펫박스 데이터 분석  

고양이와 강아지 용품을 파는 사이트에 주문 데이터를 사용한 데이터 분석입니다.  

주문 데이터를 분석하여 특징을 파악하고, 연관분석을 통해 어떠한 상품들을 연관 지어서 구매하는지에 대해 분석해 보았습니다.  

## ❓ 연관 분석이란?

- 데이터에 숨겨진 항목간의 연관규칙을 찾아내는 분석 방법으로 흔히 **장바구니 분석(market basket analysis)** 이라고도 합니다.
- 연관분석은 실제로 월마트, 아마존 등 여러기업에서 마케팅에 많이 활용을 하고 있습니다.  

## :wrench: 분석 도구

- R프로그래밍

## :hammer: 주 사용 패키지  

- ggplot2 : 시각화를 위해 사용한 패키지입니다. 
- stringi, stringr : 데이터 전처리를 위해 사용한 패키지입니다.  
- arules: 연관분석에 사용되는 패키지입니다.

## :computer: 분석 결과  

### 카테고리 분석 & 시간대별 분석

#### 1-1 대분류[댕댕이와 냥냥이] 구매량 
![image](https://user-images.githubusercontent.com/55734436/104839618-95ed5680-5905-11eb-9be4-e7d37caa4303.png)

> 대분류에서 댕댕이와 냥냥이의 구매량이 상대적으로 많은 관계로 따로 분류를 해서 그래프를 그렸습니다. 댕댕이>냥냥이 순으로 댕댕이가 냥냥이보다 약 3배 정도 더 많은 구매량을 나타내는 것을 알 수 있습니다.

#### 1-2 대분류[그외] 구매량
![image](https://user-images.githubusercontent.com/55734436/104839649-bfa67d80-5905-11eb-9d75-c27fe5b474f3.png)

> 댕댕이와 냥냥이를 제외한 다른 분류에 구매량을 비교해 보았을 때, 사은품>일반상품>소셜상품>박스>애견 등의 순으로 구매량이 많은 것을 알 수 있습니다.

