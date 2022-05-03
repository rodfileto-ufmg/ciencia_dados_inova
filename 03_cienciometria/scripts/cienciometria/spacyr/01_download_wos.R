library(RSelenium)
library(purrr)

myswitch <- function (remDr, windowId) 
{
  qpath <- sprintf("%s/session/%s/window", remDr$serverURL, 
                   remDr$sessionInfo[["id"]])
  remDr$queryRD(qpath, "POST", qdata = list(handle = windowId))
}




rstudioapi::terminalExecute('java -Dwebdriver.chrome.driver="chromedriver" -jar selenium-server-standalone-3.9.1.jar')

remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4444L,
  browserName = "chrome"
)

remDr$getStatus()

remDr$open()

remDr$navigate("https://sistemas.ufmg.br/idp/login.jsp")

# try to switch to new window
check_handle <- FALSE
count <- 0
while(!check_handle || count > 20){
  count <- count + 1
  windows_handles <- remDr$getWindowHandles()
  if(length(windows_handles) < 2){
    Sys.sleep(1)
  }else{
    check_handle <- TRUE
  }
}
myswitch(remDr, windows_handles[[3]])

# remDr$navigate("https://www-webofscience.ez27.periodicos.capes.gov.br/wos/woscc/summary/05117817-fbd1-4a46-b2b6-e5b02be8baad-151faae3/relevance/1")
# biometric OR biometrics
# https://www-webofscience.ez27.periodicos.capes.gov.br/wos/woscc/summary/39cb1c23-e477-491b-b68e-0ecbcb74f3ea-15a4017d/relevance/1
#50 965

#forensic OR forensics
# https://www.webofscience.com/wos/woscc/summary/64a13a08-5127-44e5-8b14-c26f22475dd6-15a90d8c/relevance/1
# 1 exclude years 2021 and 2020 and 2019
# 2 years 2019 2020 2021

# (facial identification) OR (facial recognition) OR criminalistic*

# fingerprint or fingermark
# https://www.webofscience.com/wos/woscc/summary/201c0f41-c9f7-48ca-aadd-7275a8a135c7-1610f9d4/relevance/1

#hyperspectral imag*

#hyperspectral image


results <- 31721

results/500

start_register <- seq(from = 1, to = results, by = 500)

end_register <- seq(from = 500, to = results, by = 500)

end_register <- c(end_register, results)

n_files <- length(start_register)


download_wos <- function(start_register, end_register) {

export <- remDr$findElement(using = "xpath", value =  '//*[@id="snRecListTop"]/app-export-menu/div/button/span[1]')

Sys.sleep(3)

export$clickElement()

Sys.sleep(3)

file_format <- remDr$findElement(using = "xpath", value = '//*[@id="exportToBibtexButton"]')

Sys.sleep(3)

file_format$clickElement()

Sys.sleep(3)

records <- remDr$findElement(using = "xpath", value = '//*[@id="radio3"]/label/span[1]')

Sys.sleep(3)

records$clickElement()

Sys.sleep(3)

records_1 <- remDr$findElement(using = "xpath", value = '/html/body/app-wos/div/div/main/div/div[2]/app-input-route[1]/app-export-overlay/div/div[3]/div[2]/app-export-out-details/div/div[2]/div/fieldset/mat-radio-group/div[3]/mat-form-field[1]/div/div[1]/div[3]/input')

Sys.sleep(3)

records_1$clearElement()

Sys.sleep(3)

records_1$sendKeysToElement(list(as.character(start_register)))

Sys.sleep(3)

records_2 <- remDr$findElement(using = "xpath", value = '/html/body/app-wos/div/div/main/div/div[2]/app-input-route[1]/app-export-overlay/div/div[3]/div[2]/app-export-out-details/div/div[2]/div/fieldset/mat-radio-group/div[3]/mat-form-field[2]/div/div[1]/div[3]/input')

Sys.sleep(3)

records_2$clearElement()

Sys.sleep(3)

records_2$sendKeysToElement(list(as.character(end_register)))

Sys.sleep(3)

content <- remDr$findElement(using = "xpath", value = '/html/body/app-wos/div/div/main/div/div[2]/app-input-route[1]/app-export-overlay/div/div[3]/div[2]/app-export-out-details/div/div[2]/div/div[1]/wos-select/button/span[1]')

Sys.sleep(3)

content$clickElement()

Sys.sleep(3)

content_full <- remDr$findElement(using = "xpath", value = '/html/body/app-wos/div/div/main/div/div[2]/app-input-route[1]/app-export-overlay/div/div[3]/div[2]/app-export-out-details/div/div[2]/div/div[1]/wos-select/div/div/div[2]/div[4]/span')

Sys.sleep(3)

content_full$clickElement()

Sys.sleep(3)

export <- remDr$findElement(using = "xpath", value =  '/html/body/app-wos/div/div/main/div/div[2]/app-input-route[1]/app-export-overlay/div/div[3]/div[2]/app-export-out-details/div/div[2]/div/div[2]/button[1]/span[1]/span')

Sys.sleep(3)

export$clickElement()

Sys.sleep(30)


}


pmap(list(start_register, end_register), download_wos)






# 
# 
# # file_format <- remDr$findElement(using = "xpath", value = '//*[@id="saveToMenu"]/li[4]/a')
# # 
# # file_format$clickElement()
# 
# records <- remDr$findElement(using = "xpath", value = '//*[@id="numberOfRecordsRange"]')
# 
# records$clickElement()
# 
# records_1 <- remDr$findElement(using = "xpath", value = '//*[@id="markFrom"]')
# 
# records_1$clearElement()
# 
# records_1$sendKeysToElement(list("2000"))
# 
# records_2 <- remDr$findElement(using = "xpath", value = '//*[@id="markTo"]')
# 
# records_2$clearElement()
# 
# records_2$sendKeysToElement(list("2500"))
# 
# content <- remDr$findElement(using = "xpath", value = '//*[@id="select2-bib_fields-container"]')
# 
# content$clickElement()
# 
# 
# content_full <- remDr$findElement(using = "xpath", value = '/html/body/span/span/span[2]/ul/li[4]')
# 
# content_full$clickElement()
# 
# file_format <- remDr$findElement(using = "xpath", value = '//*[@id="select2-saveOptions-container"]')
# 
# file_format$clickElement()
# 
# file_format_bibtex <- remDr$findElement(using = "xpath", value = '/html/body/span/span/span[2]/ul/li[2]')
# 
# file_format_bibtex$clickElement()
# 
# export_file <- remDr$findElement(using = "xpath", value = '//*[@id="exportButton"]')
# 
# export_file$clickElement()
# 
# 
# 
remDr$close()
# 
