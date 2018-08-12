function newAuctionSpreadsheet(lotNum){

  //attempt to convert lotnumber to int and if this fails then use default value
  var lotNumber = parseInt(lotNum);
  if (isNaN(lotNumber)){
    lotNumber = 700;
  }

  //create new spreadsheet with next saturday's date and ensuring uniqueness of file name in that folder
  var fdate = formattedDate(nextDay(addDays(new Date(),1), 6));
  var fileName = 'Auction Sheet - ' + fdate;
  var fileNameUnique = getUniqueName(fileName);
  var ss = SpreadsheetApp.create(fileNameUnique);

  //rename Sheet 1 to Auctioneer Sheet
  var auctioneerSheet = ss.getSheetByName('Sheet1');
  auctioneerSheet.setName('Auction Sheet');

  //chosen row values
  var auctionRowNum = lotNumber + 1;
  var auctionColumnNum = 8;
  trimRowsCols(auctioneerSheet, auctionRowNum, auctionColumnNum);

  //set column widths for Auctioneer Sheet
  var columnWidths = [80, 70, 380, 90, 120, 90, 90, 70]; //these have to be wider than in the receipt to accomodate for filter view icons
  for (var i = 0; i < columnWidths.length; i++){
    auctioneerSheet.setColumnWidth(i+1, columnWidths[i]);
  }

  //add relevant column headers to Auctioneer Sheet
  var columnHeaders = [['Vendor', 'Lot No.', 'Item Description', 'Reserve', 'Pre-Sale Bids', 'Sale Price', 'Purchaser', 'Sale ID']];
  auctioneerSheet.getRange("A1:H1").setValues(columnHeaders);
  //add lot numbers to Auctioneer Sheet
  for (var i = 0; i < lotNumber; i++){
    var lot = auctioneerSheet.getRange(i+2, 2).setValue(i+1);
  }

  //make column headers bold
  auctioneerSheet.getRange(1, 1, 1, auctionColumnNum).setFontWeight("bold");
  //make "Lot No." "Reserve" "Pre-sale Bids" and "Sale Price" columns bold
  auctioneerSheet.getRange(1, 2, auctionRowNum).setFontWeight("bold");
  auctioneerSheet.getRange(1, 4, auctionRowNum, 3).setFontWeight("bold");

  //give "Pre-sale Bids" column red text
  auctioneerSheet.getRange(2, 5, auctionRowNum-1)
  .setFontColor("red")
  .setFontSize(8);

  //format "reserve" and "sale price" columns as british currency in Auctioneer Sheet
  auctioneerSheet.getRange(2, 4, lotNumber).setNumberFormat('"£"0.0,0');
  auctioneerSheet.getRange(2, 6, lotNumber).setNumberFormat('"£"0.0,0');

  //centre all columns by default
  auctioneerSheet.getRange(1, 1, auctionRowNum, auctionColumnNum).setHorizontalAlignment("center");
  //un-centre "Item Description" and "Pre-sale Bids" columns
  auctioneerSheet.getRange(1, 3, auctionRowNum).setHorizontalAlignment("left");
  auctioneerSheet.getRange(1, 5, auctionRowNum).setHorizontalAlignment("left");

  //freeze first row of Auctioneer Sheet
  auctioneerSheet.setFrozenRows(1);
  //add grid to Auctioneer Sheet
  auctioneerSheet.getRange(1, 1, auctionRowNum, auctionColumnNum).setBorder(true, true, true, true, true, true);
  //add thick lines to seperate last few columns
  auctioneerSheet.getRange(1, 6, auctionRowNum, 2).setBorder(null, true, null, true, null, null, 'black', SpreadsheetApp.BorderStyle.SOLID_THICK);

  //add analytics sheet
  analytics(ss);

  //share file with the gmail if specified
  var userSettings = getUserSettings();
  if (userSettings.check == 'true'){
    var shareEmail = userSettings.semail;
    ss.addEditor(shareEmail);
  }

}
