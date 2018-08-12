function getPurchaserReceipt(p, allItems){

  var useOldItems = allItems;
  var useCurItems = true;
  var useUnpaidItems = true;

  //references to current spreadsheet and auctioneer sheet
  var ss = SpreadsheetApp.getActive();
  var auctioneerSheet = ss.getSheetByName("Auction Sheet");

  //create new receipt sheet if there isn't one, delete old items if there is one
  var purchaserReceipt = ss.getSheetByName("Purchaser Receipt");
  if (purchaserReceipt == null){
    purchaserReceipt = ss.insertSheet('Purchaser Receipt');
    newReceipt(purchaserReceipt);
    purchaserReceipt.activate();
  }
  else{
    purchaserReceipt.activate();
    var itemNum = purchaserReceipt.getMaxRows() - 15;
    if (itemNum >= 1){
      purchaserReceipt.deleteRows(6, itemNum);
    }
  }

  //search the auction sheet for all rows containing items the purchaser bought and their maximum SaleID
  var purchaserNum = parseInt(p);
  var purchaserCur;

  var maxSaleID = 0;
  var curSaleID;

  var items = [];

  for (var row = 2; row <= auctioneerSheet.getMaxRows(); row++){

    purchaserCur = auctioneerSheet.getRange(row, 7).getValue();
    if (purchaserCur == purchaserNum){

      items.push(row);

      curSaleID = auctioneerSheet.getRange(row, 8).getValue();
      if (curSaleID > maxSaleID){
       maxSaleID = curSaleID;
      }

    }
  }

  //take rows and split it into 3 arrays depending on whether item already paid for (sale ID < rows[0]), being paid for (sale ID == rows[0]) or not paid for (sale ID is empty)
  var oldItems = [];
  var curItems = [];
  var unpaidItems = [];

  for (var i = 0; i < items.length; i++){
    curSaleID = auctioneerSheet.getRange(items[i], 8);
    if (curSaleID.isBlank()){
      unpaidItems.push(items[i]);
    }
    else if (curSaleID.getValue() == maxSaleID){
      curItems.push(items[i]);
    }
    else{
      oldItems.push(items[i]);
    }
  }

  //push items from each of the respective arrays onto the receipt depending on the settings at the top
  if (useOldItems && oldItems.length > 0){
    pushReceipt(purchaserReceipt, oldItems, auctioneerSheet);
  }
  if (useCurItems && curItems.length > 0){
    pushReceipt(purchaserReceipt, curItems, auctioneerSheet);
  }
  if (useUnpaidItems && unpaidItems.length > 0){
    pushReceipt(purchaserReceipt, unpaidItems, auctioneerSheet);
  }

  //set totals
  var rowNum = purchaserReceipt.getMaxRows();
  var itemNum = rowNum - 15;

  var value0 = [p];
  var value1 = ['=SUMIF(D6:D' + (itemNum + 5) + ',' + maxSaleID + ',C6:C' + (itemNum + 5) + ')'];
  var value2 = ['=C' + (rowNum - 7) + '*0.15'];
  var value3 = ['=C' + (rowNum - 7) + '+C' + (rowNum - 6)];
  var value4 = ['=IFERROR((SUM(FILTER(C6:C' + (itemNum + 5) + ',ISBLANK(D6:D' + (itemNum + 5) + ')))*1.15),0)'];

  purchaserReceipt.getRange(rowNum - 8, 3, 5, 1).setValues([value0, value1, value2, value3, value4]);

  //set date and saleID
  purchaserReceipt.getRange(3,1).setValue(formattedDate(new Date()));
  purchaserReceipt.getRange(rowNum,4).setValue('No. ' + maxSaleID);
}

function newReceipt(receiptSheet){
  //values and formatting for receipt template

  trimRowsCols(receiptSheet, 15, 4);

  var columnWidths = [60, 380, 70, 55];
  for (var i = 0; i < columnWidths.length; i++){
    receiptSheet.setColumnWidth(i+1, columnWidths[i]);
  }

  var userSettings = getUserSettings();

  receiptSheet.getRange(1, 1, 3, 1)
  //.setValues([['Community Auctions Receipt'],['11 Miller Court, Miller Business Park, Station Road, Liskeard, PL14 4DA'],['00/00/0000']])
  .setValues([[userSettings.name + ' Receipt'],[userSettings.address],['00/00/0000']])
  .setFontWeight("bold");

  receiptSheet.getRange(5, 1, 1, 4)
  .setValues([['Lot No.', 'Item Description', 'Sale Price', 'Sale ID']])
  .setFontWeight("bold")
  .setBorder(true, true, true, true, true, true)
  .setHorizontalAlignment("center");

  receiptSheet.getRange(7, 2, 1, 2)
  .setValues([['Purchaser Summary For', '0']])
  .setFontWeight("bold")
  .setBorder(true, true, true, true, false, false);

  receiptSheet.getRange(8, 2, 4, 1)
  .setValues([['Total Of Items Bought'],["Buyer's Premium (15%)"],['Total Due'],['Amount Left Unpaid (Including Premium)']])
  .setBorder(true, true, true, true, false, false);

  receiptSheet.getRange(8, 3, 4, 1)
  .setValues([['0'],['0'],['0'],['0']])
  .setFontWeight("bold")
  .setBorder(true, true, true, true, false, false)
  .setNumberFormat('"£"0.0,0')
  .setHorizontalAlignment("center");

  receiptSheet.getRange(13, 1, 3, 1)
  .setValues([['Web:'],['Tel:'],['Email:']])
  .setFontWeight("bold")
  .setHorizontalAlignment("right");

  receiptSheet.getRange(13, 2, 3, 1)
  //.setValues([['www.communityauctions.co.uk'],['01579 20 82 50'],['communityauctions@mail.com']])
  .setValues([[userSettings.website],[userSettings.telephone],[userSettings.email]])
  .setFontWeight("bold")
  .setFontColors([['black'],['black'],['black']])
  .setHorizontalAlignment("left");

  receiptSheet.getRange(15, 4)
  .setValues([['No. 0']])
  .setFontWeight("bold");

  receiptSheet.getRange(1, 1).setFontSize(18);
  receiptSheet.getRange(7, 3).setHorizontalAlignment("center");
  receiptSheet.getRange(5, 2).setHorizontalAlignment("left");
  receiptSheet.getRange(13, 2).setFontColor('black');
}

function pushReceipt(receiptSheet, items, auctioneerSheet){
  //add new rows for new items
  receiptSheet.insertRowsBefore(receiptSheet.getMaxRows()-9, items.length);

  //copy over new items
  var columns = [2, 3, 6, 8];
  var startRow = receiptSheet.getMaxRows()-9 - items.length;
  for (var j = 0; j < items.length; j++){
    for (var i = 0; i < columns.length; i++){
      auctioneerSheet.getRange(items[j], columns[i]).copyTo(receiptSheet.getRange(startRow + j, i+1), {contentsOnly:true});
    }
  }

  //format new items
  receiptSheet.getRange(startRow, 1, items.length, 4)
  .setHorizontalAlignment("center")
  .setBorder(true, true, true, true, true, true);

  receiptSheet.getRange(startRow, 3, items.length, 1)
  .setFontWeight("bold")
  .setNumberFormat('"£"0.0,0');

  receiptSheet.getRange(startRow, 2, items.length, 1).setHorizontalAlignment("left");
  receiptSheet.getRange(startRow, 1, items.length, 1).setFontWeight("bold");
}
