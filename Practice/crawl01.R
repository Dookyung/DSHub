#########################################
# 국토교통부 실거래가 데이터 입수
########################################

### Python Code Replication

library(httr)

URL = 'http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade'

SERVICEKEY = 'roZZFFdy8FI2tEyXyvmqrvWEDc9Vh41yDFzQjbk0nRb270GEhCKGd1MdN4%2Bx1CC2PXhmL8L3ya2xOuueUljDtg%3D%3D' 
YYMM <- '201512'
REGION <- '11110'

URL_call = paste0(URL,"?LAWD_CD=",REGION,"&DEAL_YMD=",YYMM,"&serviceKey=",SERVICEKEY)	

request <- GET(URL_call, add_headers(Accept = "application/xml"))
#request <- GET(URL_call)
response_body <- content(request, "text", encoding = "UTF-8")

response_body

writeLines(response_body, "./#practice/test_data.xml", useBytes=T)

