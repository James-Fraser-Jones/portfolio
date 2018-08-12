function trimRowsCols(sheet, rowNum, columnNum){
  //default row and column values
  var defaultRowNum = 1000;
  var defaultColumnNum = 26;

  //add or remove rows for Sheet
  if (rowNum < defaultRowNum){
    sheet.deleteRows(1, defaultRowNum-rowNum);
  }
  else if (defaultRowNum < rowNum){
    sheet.insertRows(1, rowNum-defaultRowNum);
  }
  //add or remove columns for Auctioneer Sheet
  if (columnNum < defaultColumnNum){
    sheet.deleteColumns(1, defaultColumnNum-columnNum);
  }
  else if (defaultColumnNum < columnNum){
    sheet.insertColumns(1, columnNum-defaultColumnNum);
  }
}

//efficiently get array of unique numbers from "a"
function uniq_fast(a) {
  var seen = {};
  var out = [];
  var len = a.length;
  var j = 0;
  for(var i = 0; i < len; i++) {
    var item = a[i];
    if(seen[item] !== 1) {
      seen[item] = 1;
      out[j++] = item;
    }
  }
  return out;
}

function formattedDate(date){
  //var date = new Date()
  var fdate = date.getDate().toString() + '/' + (date.getMonth() + 1).toString() + '/' + date.getYear().toString();
  return fdate;
}

function nextDay(date, x){
  date.setDate(date.getDate() + (x+(7-date.getDay())) % 7);
  return date;
}

function addDays(date, days) {
  var result = new Date(date);
  result.setDate(result.getDate() + days);
  return result;
}

//these functions do get "shared with me" files but not files in the trash
function getFilesByName(name){
  var fileI = DriveApp.getFilesByName(name);
  var file;
  var files = [];
  while (fileI.hasNext()) {
    file = fileI.next();
    files.push(file);
  }
  return files;
}

function fileExists(name){
  var fileI = DriveApp.getFilesByName(name);
  if (fileI.hasNext()) {
    return true
  }
  else{
    return false
  }
}

function getUniqueName(name){
  var i = 1;
  var uniqueName = name;
  var fileI = DriveApp.getFilesByName(uniqueName);

  while(fileI.hasNext()){
    i++;
    uniqueName = name + ' - ' + i
    fileI = DriveApp.getFilesByName(uniqueName);
  }

  return uniqueName;
}
