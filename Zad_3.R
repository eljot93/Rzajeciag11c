#1.Utworz funkcje: rankAccount <- function(dataFrame,colName,groupName,valueSort,num)  
#  ktora bedzie zwracala dla danej tabeli(dataFrame) n wierszy posiadajace najwieksze 
#  wartosci(sortowanie po kolumnie valueSort)  dla wybranej grupy (konkretna wartosc komorki ,np
#NAUCZYCIEL) z kolumny(colName) np. occupation-zawod. """

rankAccount <- function(dataFrame,colName,groupName,valueSort,num) {
  

  num <- readline("Podaj ilosc wierszy ")  
  groupName <- readline("Podaj zawod ")  

  
  colName <-  c("Dostawca","Hydraulik","Nauczyciel","Hydraulik","Monter","Hydraulik") 
  valueSort <-  c(200,300,100,300,200,500)
  
  
  
  dataFrame <- data.frame(colName, valueSort)
  dataFrame <- dataFrame[order(dataFrame$valueSort,decreasing = TRUE),]
  
  
  newdataframe <- dataFrame[which(dataFrame$colName == groupName),]
  head(newdataframe,num)
}

rankAccount(dataFrame = dataFrame, colName = colName, valueSort = valueSort)


#2.Tak jak w 1 tylko z uzyciem datachunku.  
#przyklad naglowka: 

  rankAccountBigDatatoChunk(filename = "usersAccounts.csv", 1000,  
                            "occupation", "NAUCZYCIEL", "saldo",10) 





#3.SPRAWIDZIC CZY DA SIE ZROBIC TO SAMO W zapytaniu SQL dla takich wartosci jak: 
#  tabelaZbazyDanych,occupation, nauczyciel, saldo 


library(DBI)
library(RSQLite)



readToBase<-function(filepath,dbpath,tablename,size,sep=",",header=TRUE,delete=TRUE){
  ap<-!delete
  ov<-delete
  fileConnection<- file(description = filepath,open="r")
  dbConn<-dbConnect(SQLite(),dbpath)
  data<-read.table(fileConnection,nrows=size,header = header,fill=TRUE,sep=sep)
  columnsNames<-names(data)
  dbWriteTable(conn = dbConn,name=tablename,data,append=ap,overwrite=ov)
  
  repeat{
    if(nrow(data)==0){
      close(fileConnection)
      dbDisconnect(dbConn)
      break
    }
    data<-read.table(fileConnection,nrows=size,col.names=columnsNames,fill=TRUE,sep=sep)
    dbWriteTable(conn = dbConn,name=tablename,data,append=TRUE,overwrite=FALSE)
  }
}
readToBase("konta.csv","praca3.SQLite", "praca3",1000)


SQLFunction <- function(tabelaZbazyDanych, occupation, saldo){
  dbp <- "praca3.sqlite"
  con <- dbConnect(SQLite(),dbp)
  dbGetQuery(con, paste0("SELECT * FROM ",tabelaZbazyDanych," WHERE occupation = '",occupation,"' ORDER BY ",saldo," DESC"))
}


SQLFunction("praca3", "NAUCZYCIEL", "saldo")






