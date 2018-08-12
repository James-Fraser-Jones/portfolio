function createUnpaidReceipts(){

  //references to current spreadsheet and auctioneer sheet
  var ss = SpreadsheetApp.getActive();
  var auctioneerSheet = ss.getSheetByName("Auction Sheet");

  //create a sorted list of unpaid purchasers
  var unpaidPurchasers = [];

  var curSaleID;
  var purchaserCur;
  for (var row = 2; row <= auctioneerSheet.getMaxRows(); row++){

    curSaleID = auctioneerSheet.getRange(row, 8);
    if (curSaleID.isBlank()){
      purchaserCur = auctioneerSheet.getRange(row, 7);
      if (!purchaserCur.isBlank()){
        unpaidPurchasers.push(purchaserCur.getValue());
      }
    }

  }
  unpaidPurchasers = uniq_fast(unpaidPurchasers);
  unpaidPurchasers.sort(function(a, b){return a-b});

  //Create filename and delete any files with the same name
  var fdate = formattedDate(new Date());
  var fileName = "Purchaser Receipts - " + fdate;
  var fileNameUnique = getUniqueName(fileName);

  //Create a fresh spreadsheet with the new filename
  var ssnew = SpreadsheetApp.create(fileNameUnique);

  //Create a new sheet for creating receipts and add the template
  var unpaidReceipt = ss.insertSheet("Purchaser Receipts");
  newUnpaidReceipt(unpaidReceipt);

  //Create receipt for each unpaid purchaser and add it to the new file, also rename the sheet to the purchasers number and delete items after copying, ready for next receipt items
  for(var i = 0; i < unpaidPurchasers.length; i++){
    getUnpaidReceipt(unpaidReceipt, unpaidPurchasers[i], auctioneerSheet);
    var newSheet = ss.getSheetByName('Purchaser Receipts').copyTo(ssnew);
    newSheet.setName(unpaidPurchasers[i]);

    var itemNum = unpaidReceipt.getMaxRows() - 17;
    if (itemNum >= 1){
      unpaidReceipt.deleteRows(6, itemNum);
    }
  }

  //Remove sheet1 and also remove Unpaid Receipts sheet
  ssnew.deleteSheet(ssnew.getSheetByName('Sheet1'));
  ss.deleteSheet(unpaidReceipt);

  /*
  //share file with the group
  var groupEmail = 'communityauctions@googlegroups.com';
  ssnew.addEditor(groupEmail);
  */
}

function getUnpaidReceipt(unpaidReceipt, p, auctioneerSheet){

  //search the auction sheet for all rows containing items the purchaser bought and didn't pay for
  var purchaserNum = parseInt(p);
  var purchaserCur;

  var curSaleID;

  var unpaidItems = [];

  for (var row = 2; row <= auctioneerSheet.getMaxRows(); row++){

    purchaserCur = auctioneerSheet.getRange(row, 7).getValue();
    if (purchaserCur == purchaserNum){

      curSaleID = auctioneerSheet.getRange(row, 8);
      if (curSaleID.isBlank()){
        unpaidItems.push(row);
      }

    }
  }

  if (unpaidItems.length > 0){
    pushUnpaidReceipt(unpaidReceipt, unpaidItems, auctioneerSheet);
  }

  //set totals
  var rowNum = unpaidReceipt.getMaxRows();
  var itemNum = rowNum - 17;

  var value0 = [p];
  var value1 = ['=SUM(C6:C' + (itemNum + 5) + ')'];
  var value2 = ['=C' + (rowNum - 9) + '*0.15'];
  var value3 = ['=C' + (rowNum - 9) + '+C' + (rowNum - 8)];

  unpaidReceipt.getRange(rowNum - 10, 3, 4, 1).setValues([value0, value1, value2, value3]);

  //set date
  unpaidReceipt.getRange(3,1).setValue(formattedDate(new Date()));

  //add contact name and information
  var dbFiles = getFilesByName('Customer Database');
  if (dbFiles.length >= 1){

    var dbSheet = SpreadsheetApp.open(dbFiles[0]).getSheets()[0];
    var purchaserInfo = [];
    var curCustomer;

    for (var row = 2; row <= dbSheet.getMaxRows(); row++){
      curCustomer = dbSheet.getRange(row, 1).getValue();
      if (curCustomer == p){
        purchaserInfo.push((dbSheet.getRange(row, 2).getValue()) + ' ' + (dbSheet.getRange(row, 3).getValue()));
        purchaserInfo.push(dbSheet.getRange(row, 4).getValue());
        break;
      }
    }

    unpaidReceipt.getRange(rowNum-5, 2).setValue(purchaserInfo[0]);
    unpaidReceipt.getRange(rowNum-4, 2).setValue(purchaserInfo[1]);

  }
}

function newUnpaidReceipt(receiptSheet){
  //values and formatting for receipt template

  trimRowsCols(receiptSheet, 17, 4);

  var columnWidths = [60, 380, 70, 55];
  for (var i = 0; i < columnWidths.length; i++){
    receiptSheet.setColumnWidth(i+1, columnWidths[i]);
  }

  var userSettings = getUserSettings();

  receiptSheet.getRange(1, 1, 3, 1)
  //.setValues([['Community Auctions Receipt'],['11 Miller Court, Miller Business Park, Station Road, Liskeard, PL14 4DA'],['00/00/0000']])
  .setValues([[userSettings.name + ' Receipt'],[userSettings.address],['00/00/0000']])
  .setFontWeight("bold");

  receiptSheet.getRange(5, 1, 1, 3)
  .setValues([['Lot No.', 'Item Description', 'Sale Price']])
  .setFontWeight("bold")
  .setBorder(true, true, true, true, true, true)
  .setHorizontalAlignment("center");

  receiptSheet.getRange(7, 2, 1, 2)
  .setValues([['Purchaser Summary For', '0']])
  .setFontWeight("bold")
  .setBorder(true, true, true, true, false, false);

  receiptSheet.getRange(8, 2, 3, 1)
  .setValues([['Total Of Items Bought'],["Buyer's Premium (15%)"],['Total Due']])
  .setBorder(true, true, true, true, false, false);

  receiptSheet.getRange(8, 3, 3, 1)
  .setValues([['0'],['0'],['0']])
  .setFontWeight("bold")
  .setBorder(true, true, true, true, false, false)
  .setNumberFormat('"£"0.0,0')
  .setHorizontalAlignment("center");

  receiptSheet.getRange(12, 1, 2, 1)
  .setValues([['Name:'],['Phone:']])
  .setFontWeight("bold")
  .setHorizontalAlignment("right");

  receiptSheet.getRange(12, 2, 2, 1).setHorizontalAlignment("left");

  receiptSheet.getRange(15, 1, 3, 1)
  .setValues([['Web:'],['Tel:'],['Email:']])
  .setFontWeight("bold")
  .setHorizontalAlignment("right");

  receiptSheet.getRange(15, 2, 3, 1)
  //.setValues([['www.communityauctions.co.uk'],['01579 20 82 50'],['communityauctions@mail.com']])
  .setValues([[userSettings.website],[userSettings.telephone],[userSettings.email]])
  .setFontWeight("bold")
  .setFontColors([['black'],['black'],['black']])
  .setHorizontalAlignment("left");

  receiptSheet.getRange(17, 3)
  .setValues([['Post-Auction']])
  .setFontWeight("bold");

  receiptSheet.getRange(1, 1).setFontSize(18);
  receiptSheet.getRange(7, 3).setHorizontalAlignment("center");
  receiptSheet.getRange(5, 2).setHorizontalAlignment("left");
}

function pushUnpaidReceipt(receiptSheet, items, auctioneerSheet){
  //add new rows for new items
  receiptSheet.insertRowsBefore(receiptSheet.getMaxRows()-11, items.length);

  //copy over new items
  var columns = [2, 3, 6];
  var startRow = receiptSheet.getMaxRows()-11 - items.length;
  for (var j = 0; j < items.length; j++){
    for (var i = 0; i < columns.length; i++){
      auctioneerSheet.getRange(items[j], columns[i]).copyTo(receiptSheet.getRange(startRow + j, i+1), {contentsOnly:true});
    }
  }

  //format new items
  receiptSheet.getRange(startRow, 1, items.length, 3)
  .setHorizontalAlignment("center")
  .setBorder(true, true, true, true, true, true);

  receiptSheet.getRange(startRow, 3, items.length, 1)
  .setFontWeight("bold")
  .setNumberFormat('"£"0.0,0');

  receiptSheet.getRange(startRow, 2, items.length, 1).setHorizontalAlignment("left");
  receiptSheet.getRange(startRow, 1, items.length, 1).setFontWeight("bold");
}
