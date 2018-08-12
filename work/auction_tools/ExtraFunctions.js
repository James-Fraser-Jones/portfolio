function import(){
  //references to current spreadsheet and auctioneer sheet
  var ss = SpreadsheetApp.getActive();
  var auctioneerSheet = ss.getSheetByName("Auction Sheet");
  var importSheet = ss.getSheetByName("Import");

  var importRowNum = importSheet.getMaxRows();
  var auctionRowNum = auctioneerSheet.getMaxRows();

  //delete lot numbers from auction sheet
  auctioneerSheet.getRange(2, 2, auctionRowNum-1, 1).clearContent();

  /*
  //import new lot numbers into auction sheet
  importSheet.getRange(1,2,importRowNum,1).copyTo(auctioneerSheet.getRange(2,1,importRowNum,1), {contentsOnly:true});

  //import new vendor numbers into auction sheet
  importSheet.getRange(1,1,importRowNum,1).copyTo(auctioneerSheet.getRange(2,2,importRowNum,1), {contentsOnly:true});

  //import everything else into auction sheet
  importSheet.getRange(1,3,importRowNum,3).copyTo(auctioneerSheet.getRange(2,3,importRowNum,3), {contentsOnly:true});
  */

  //import everything into auction sheet
  importSheet.getRange(1,1,importRowNum,5).copyTo(auctioneerSheet.getRange(2,1,importRowNum,5), {contentsOnly:true});

}

function catalogue(){
  //references to current spreadsheet and auctioneer sheet
  var ss = SpreadsheetApp.getActive();
  var auctioneerSheet = ss.getSheetByName("Auction Sheet");

  var catalogue = ss.getSheetByName('Catalogue');
  if (catalogue != null){
    ss.deleteSheet(catalogue);
  }
  auctioneerSheet.activate();
  catalogue = ss.duplicateActiveSheet();
  catalogue.setName('Catalogue');

  catalogue.deleteColumns(4,5);
  catalogue.deleteColumn(1);

  var cataRowNum = catalogue.getMaxRows();
  catalogue.getRange(1, 1, cataRowNum, 2)
  .setFontSize(14)
  .setFontFamily('Calibri');

  catalogue.activate();
}

function analytics(ss){
  //reference to current spreadsheet
  if (ss == null){
    ss = SpreadsheetApp.getActive();
  }

  //delete analytics sheet if it already exists
  var analytics = ss.getSheetByName('Analytics');
  if (analytics != null){
    ss.deleteSheet(analytics);
  }

  //make a new sheet with correct row and column numbers
  analytics = ss.insertSheet('Analytics');
  trimRowsCols(analytics, 17, 7);

  //add analytics sheet values
  analytics.getRange(1, 1).setValue('Analytics');
  analytics.getRange(3, 1, 4, 2).setValues([['Hammer Price', "=SUM('Auction Sheet'!F2:F)"],['Money In', '=B3*1.15'],['Money Out', '=B3 - SUMIF(\'Auction Sheet\'!F2:F, ">6.66")*0.15 - COUNTIF(\'Auction Sheet\'!F2:F, "<6.67")'],['Profit', '=B4-B5']]);
  analytics.getRange(8, 1, 4, 2).setValues([['Avg customer spend', '=IFERROR(AVERAGE(E4:E)*1.15, 0)'],['Greatest customer spend', '=MAX(E4:E)*1.15'],['Avg vendor sellings', '=IFERROR(AVERAGE(G4:G)*0.85, 0)'],['Greatest vendor sellings', '=Max(G4:G)*0.85']]);
  analytics.getRange(13, 1, 2, 2).setValues([['Unique Customers', '=COUNTA(D4:D)'],['Percentage Sold', "=IFERROR(COUNTA('Auction Sheet'!G2:G)/COUNTA('Auction Sheet'!C2:C), 0)"]]);
  analytics.getRange(16, 1, 2, 1).setValues([['Key:'],['commission/premium included']]);
  analytics.getRange(3, 4, 2, 4).setValues([['Purchaser','Total Spent','Vendor','Total Sold'],["=Sort(Unique('Auction Sheet'!G2:G))","=ARRAYFORMULA(IF(D4:D,SUMIF('Auction Sheet'!G2:G, D4:D, 'Auction Sheet'!F2:F),\"\"))","=Sort(Unique('Auction Sheet'!A2:A))","=ARRAYFORMULA(IF(F4:F,SUMIF('Auction Sheet'!A2:A, F4:F, 'Auction Sheet'!F2:F),\"\"))"]]);

  //add analytics sheet formatting
  var analyticsRowNum = analytics.getMaxRows();
  var analyticsColumnNum = 7;

  var columnWidths = [300, 140, 140, 140, 140, 140, 140];
  for (var i = 0; i < columnWidths.length; i++){
    analytics.setColumnWidth(i+1, columnWidths[i]);
  }

  analytics.getRange(1,1,analyticsRowNum,analyticsColumnNum).setFontSize(18);

  analytics.getRange(1,1,17,1).setFontWeight('bold');
  analytics.getRange(3,4,1,4).setFontWeight('bold');

  analytics.getRange(3,2,9,1).setNumberFormat('"£"0.0,0');
  analytics.getRange(4,5,analyticsRowNum-3,1).setNumberFormat('"£"0.0,0');
  analytics.getRange(4,7,analyticsRowNum-3,1).setNumberFormat('"£"0.0,0');

  analytics.getRange(14,2).setNumberFormat('0.##%');

  analytics.getRange(3,1,4,2).setBorder(true, true, true, true, true, true);
  analytics.getRange(8,1,4,2).setBorder(true, true, true, true, true, true);
  analytics.getRange(13,1,2,2).setBorder(true, true, true, true, true, true);

  var greenColour = '#00c400';
  analytics.getRange(4,2,2,1).setFontColor(greenColour);
  analytics.getRange(8,2,4,1).setFontColor(greenColour);
  analytics.getRange(17,1).setFontColor(greenColour);

  //hide columns and show analytics sheet
  analytics.hideColumns(4,4);
  analytics.activate();
}
