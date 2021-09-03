#######################################
###professor Choi, shizu@snu.ac.kr ####
#######################################




#######################################
########   R 데이터 구조    ###########
#######################################

#scalr#vector#list#array#data.frame#c#rbind#cbind#$




#ctrl+enter 누르면 console 창에 출력, 여러줄 선택해서 run도 가능
#함수는 yellow로 표시, 괄호 안에 함수에 넣을 것을 지정
#comment를 쓸때는 앞에 #을 삽입
print("Hello world!")
#모든 계산 가능
1*2
3*4
2/4

#변수(variable) 만들기
#왼쪽이 객체, 오른쪽은 투입할 데이터 (순서에 유의하세요)
a<-2
a
a<-3
a
#concatenate의 약자 c, 연결의 의미
a<-c(3,5)
a

#scalar, vector, array, list, dataframe의 이해
#scalar: 하나의 원소(element)
scalar<-1
scalar
scalar<-"bts"
scalar

#vector : 여러개의 원소들이나 하나의 row
vector <-c(1,2,3)
vector
vector <-c("v", "rm", "suga")
vector

#matrix : 2*2, 2*3의 행렬 (vector를 여러개의 row로 쌓은형태)
matrix <-matrix(c(1,2,3,4,5,6), nrow=3)
matrix
matrix <-matrix(c(1,2,3,4,5,6), nrow=2)
matrix
matrix <-matrix(c(1,2,3,4,5,6), nrow=2, byrow=TRUE)
matrix
matrix <-matrix(c(1:20), nrow=4, ncol=5)
matrix
mat1 <-c(1:3)
mat2 <-c(4:6)
matrix<-c(mat1, mat2)
matrix
matrix <-cbind(mat1, mat2)  #cbind : column을 기준으로 횡으로 붙이기
matrix
matrix <-rbind(mat1, mat2)  #rbind : row을 기준으로 종으로 붙이기
matrix
#특정 위치의 요소 추출 및 치환
matrix[1,2]
matrix[1:2]
matrix[1,] #첫번째 row의 모든 원소를 추출
matrix[,1] #첫번째 col의 모든 원소를 추출
matrix[c(1,2),] #1,2번째 row의 모든 원소를 추출
matrix[1,2]=100
matrix

#array : matrix를 여러층으로 쌓은것
matrix1<- matrix(c(1:9), nrow=3)
matrix1
matrix2<- matrix(c(10:18), nrow=3)
matrix3<- matrix(c(19:27), nrow=3)
matrix2
matrix3
array <-array(c(matrix1, matrix2, matrix3), dim=c(3,3,3))
array

#지금까지 살펴본 vector, matrix, array는 모두 같은 특성의 데이터로만 구성되어 있음. 즉 character, logic, numeric의 한종류
#일반적으로 쓰는 데이터는 문자변수, 숫자변수 등이 하나의 데이터셋에 담겨있음. 이 경우 쓰는 것이 dataframe. 앞으로 우리가 쓰는 대부분의 데이터는 dataframe일 것임



btsname <-c("RM", "Jin", "suga","jhope", "jimin", "V", "JK")
btsyear <-c(1994, 1992, 1993, 1994, 1995, 1995, 1997)
btsposition <-c("rap", "vocal", "rap", "rap", "vocal", "vocal","vocal")
bts <-data.frame(btsname, btsyear, btsposition)
bts   
str(bts)

bts <-data.frame(btsname, btsyear, btsposition, stringsAsFactors = TRUE)
str(bts)

#factor의 이해
#factor란 주로 categorical한 변수로서 "값"(일반벡터)에 "level"이라는 정보를 추가한 것
gender=factor(c("male", "female", "female", "male"))
gender
str(gender)
gender=factor(gender, 
              levels=c("male", "female"))
gender
str(gender)

#dataframe 활용
#변수 선택 $표시 활용
bts$btsname
bts$age <- 2021-bts$btsyear+1
bts
bts$null <-NULL
bts
bts$na <-NA


dim(bts)

