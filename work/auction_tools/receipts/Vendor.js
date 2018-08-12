function standaloneVendor(v){
  //references to current spreadsheet and auctioneer sheet
  var ss = SpreadsheetApp.getActive();
  var auctioneerSheet = ss.getSheetByName("Auction Sheet");

  //create new receipt sheet if there isn't one, delete old items if there is one
  var vendorSummary = ss.getSheetByName("Vendor Summary");
  if (vendorSummary == null){
    vendorSummary = ss.insertSheet('Vendor Summary');
    newVendorReceipt(vendorSummary);
    vendorSummary.activate();
  }
  else{
    vendorSummary.activate();
    var itemNum = vendorSummary.getMaxRows() - 14;
    if (itemNum >= 1){
      vendorSummary.deleteRows(6, itemNum);
    }
  }

  getVendorReceipt(vendorSummary, v, auctioneerSheet);
}

function createVendorReceipts(){
  var row = 0;
  var i = 0;
  var itemNum;
  var newSheet;
  var fdate = formattedDate(new Date());

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //Step 1: Get sorted lists of vendors who sold at least 1 item, and those who didn't.

  //references to current spreadsheet and auctioneer sheet
  var ss = SpreadsheetApp.getActive();
  var auctioneerSheet = ss.getSheetByName("Auction Sheet");

  //create a sorted list of both sold vendors and unsold vendors
  var soldVendors = [];
  //var unsoldVendors = [];
  var purchaserCur;
  var vendorCur;
  var index = 0;

  //get all instances of vendors and sold vendors
  for (row = 2; row <= auctioneerSheet.getMaxRows(); row++){
    vendorCur = auctioneerSheet.getRange(row, 1);
    if (!vendorCur.isBlank()){
        //unsoldVendors.push(vendorCur.getValue());
        purchaserCur = auctioneerSheet.getRange(row, 7);
        if (!purchaserCur.isBlank()){
          soldVendors.push(vendorCur.getValue());
        }
    }
  }

  //remove duplicates and sort both lists
  //unsoldVendors = uniq_fast(unsoldVendors);
  //unsoldVendors.sort(function(a, b){return a-b});
  soldVendors = uniq_fast(soldVendors);
  soldVendors.sort(function(a, b){return a-b});

  /*
  //remove every element of soldVendors from unsoldVendors (which previously contained all vendors)
  for (i = 0; i < soldVendors.length; i++){
    index = unsoldVendors.indexOf(soldVendors[i]);
    if (index > -1) {
      unsoldVendors.splice(index, 1);
    }
  }
  //*/

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //Step 2: Generate vendor receipts.

  //Create 2 new spreadsheets, one for vendors, the other for fees
  var fileName = "Vendor Summaries - " + fdate;
  var fileNameUnique = getUniqueName(fileName);
  var ssnew = SpreadsheetApp.create(fileNameUnique);

  //Create a new sheet for creating receipts and add the template
  var vendorReceipt = ss.insertSheet("Vendor Summaries");
  newVendorReceipt(vendorReceipt);

  //Create receipt for each sold vendor and add it to the new file, also rename the sheet to the vendor's number
  for(i = 0; i < soldVendors.length; i++){
    getVendorReceipt(vendorReceipt, soldVendors[i], auctioneerSheet);

    //copy reciept over to new spreadsheet and rename correctly
    newSheet = vendorReceipt.copyTo(ssnew);
    newSheet.setName(soldVendors[i]);

    itemNum = vendorReceipt.getMaxRows() - 14;
    if (itemNum >= 1){
      vendorReceipt.deleteRows(6, itemNum);
    }
  }

  //Remove default sheets from each spreadsheet
  if (ssnew.getSheets().length > 1){
    ssnew.deleteSheet(ssnew.getSheetByName('Sheet1'));
  }
  ss.deleteSheet(vendorReceipt);

  /*
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //Step 3: Generate vendor fees.

  //Create 2 new spreadsheets, one for vendors, the other for fees
  var fileName2 = "Vendor Fees - " + fdate;
  var fileNameUnique2 = getUniqueName(fileName2);
  var ssnew2 = SpreadsheetApp.create(fileNameUnique2);

  //Create a new sheet for creating fees, add the template and add necessary modifications
  var vendorFee = ss.insertSheet("Vendor Fees");
  newVendorReceipt(vendorFee);
  vendorFee.getRange(1, 1).setValue("Lotting Fee");

  //Create receipt for each unsold vendor and add it to the new file, also rename the sheet to the vendor's number
  for(i = 0; i < unsoldVendors.length; i++){

    getVendorReceipt(vendorFee, unsoldVendors[i], auctioneerSheet);

    //copy reciept over to new spreadsheet and rename correctly
    newSheet = vendorFee.copyTo(ssnew2);
    newSheet.setName(unsoldVendors[i]);

    itemNum = vendorFee.getMaxRows() - 16;
    if (itemNum >= 1){
      vendorFee.deleteRows(6, itemNum);
    }

  }

  //Remove default sheets from each spreadsheet
  if (ssnew2.getSheets().length > 1){
    ssnew2.deleteSheet(ssnew2.getSheetByName('Sheet1'));
  }
  ss.deleteSheet(vendorFee);
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //Step 4: Add extra modifications to vendor fee sheets.

  //attempt to get database sheet
  var dbCheck = false;
  if (getFilesByName('Customer Database').length >= 1){
    var dbSheet = SpreadsheetApp.open(getFilesByName('Customer Database')[0]).getSheets()[0];
    dbCheck = true;
  }

  var curCustomer;
  var ven;

  var curSheet;
  var sheets = ssnew2.getSheets();
  for (i = 0; i < sheets.length; i++){
    curSheet = sheets[i];
    ven = curSheet.getName();

    curSheet.insertRowsBefore(curSheet.getMaxRows()-3, 3);
    curSheet.getRange(curSheet.getMaxRows()-5, 1, 2, 1)
    .setValues([["Name:"],["Phone:"]])
    .setFontWeight("bold")
    .setHorizontalAlignment("right");

    if (dbCheck){
      for (row = 2; row <= dbSheet.getMaxRows(); row++){
        curCustomer = dbSheet.getRange(row, 1).getValue();
        if (curCustomer == ven){
          curSheet.getRange(curSheet.getMaxRows()-5, 2).setValue((dbSheet.getRange(row, 2).getValue()) + ' ' + (dbSheet.getRange(row, 3).getValue()));
          curSheet.getRange(curSheet.getMaxRows()-4, 2)
          .setValue(dbSheet.getRange(row, 4).getValue())
          .setHorizontalAlignment("left");
          break;
        }
      }
    }

    curSheet.deleteRows(curSheet.getMaxRows()-11, 3);
    curSheet.deleteRows(curSheet.getMaxRows()-7, 1);
    curSheet.getRange(curSheet.getMaxRows()-7, 2, 1, 2).setBorder(true, true, true, true, true, true);
  }
  //*/
}

function getVendorReceipt(vendorReceipt, v, auctioneerSheet){

  //search the auction sheet for all rows containing items the vendor was selling (even if they weren't sold)
  var vendorNum = parseInt(v);
  var vendorCur;
  var comCalc = 0;

  var soldItems = [];

  var purchaserCur;
  for (var row = 2; row <= auctioneerSheet.getMaxRows(); row++){

    vendorCur = auctioneerSheet.getRange(row, 1).getValue();
    if (vendorCur == vendorNum){

      soldItems.push(row);

    }
  }

  //if there are any items to add
  if (soldItems.length > 0){
    //add the items
    pushVendorReceipt(vendorReceipt, soldItems, auctioneerSheet);

    //calculate the commission with the new items
    var itemPrice = 0;
    for (row = 6; row < (soldItems.length + 6); row++){
      itemPrice = vendorReceipt.getRange(row, 3);
      if (itemPrice.isBlank()){

      }
      else if (itemPrice.getValue() > 6.66){
       comCalc = comCalc + (itemPrice.getValue() * 0.15);
      }
      else{
        comCalc = comCalc + 1;
      }
    }
  }

  var rowNum = vendorReceipt.getMaxRows();
  var itemNum = rowNum - 14;

  //set values in total at bottom
  var value0 = [v];
  var value1 = ['=SUM(C6:C' + (itemNum + 5) + ')'];
  var value2 = [comCalc];
  //var value3 = ['=C' + (rowNum - 8) + '-C' + (rowNum - 7)];
  //var value4 = [itemNum * 0.5];
  var value5 = ['=C' + (rowNum - 6) + '-C' + (rowNum - 5)];
  vendorReceipt.getRange(rowNum - 7, 3, 4, 1).setValues([value0, value1, value2, value5]);

  //set date
  vendorReceipt.getRange(3,1).setValue(formattedDate(new Date()));
}

function newVendorReceipt(receiptSheet){
  //values and formatting for receipt template

  trimRowsCols(receiptSheet, 14, 4);

  var columnWidths = [60, 380, 70, 55];
  for (var i = 0; i < columnWidths.length; i++){
    receiptSheet.setColumnWidth(i+1, columnWidths[i]);
  }

  var userSettings = getUserSettings();

  receiptSheet.getRange(1, 1, 3, 1)
  .setValues([[userSettings.name + ' Vendor Summary'],[userSettings.address],['00/00/0000']])
  .setFontWeight("bold");

  receiptSheet.getRange(5, 1, 1, 3)
  .setValues([['Lot No.', 'Item Description', 'Sale Price']])
  .setFontWeight("bold")
  .setBorder(true, true, true, true, true, true)
  .setHorizontalAlignment("center");

  receiptSheet.getRange(7, 2, 1, 2)
  .setValues([['Vendor Summary For', '0']])
  .setFontWeight("bold")
  .setBorder(true, true, true, true, false, false);

  receiptSheet.getRange(8, 2, 3, 1)
  .setValues([['Total Of Items Sold'],["Vendor's Commission: 15% (min £1 per item sold)"],['Total Payable']])
  .setBorder(true, true, true, true, false, false);

  receiptSheet.getRange(8, 3, 3, 1)
  .setValues([['0'],['0'],['0']])
  .setFontWeight("bold")
  .setBorder(true, true, true, true, false, false)
  .setNumberFormat('"£"0.0,0')
  .setHorizontalAlignment("center");

  receiptSheet.getRange(12, 1, 3, 1)
  .setValues([['Web:'],['Tel:'],['Email:']])
  .setFontWeight("bold")
  .setHorizontalAlignment("right");

  receiptSheet.getRange(12, 2, 3, 1)
  .setValues([[userSettings.website],[userSettings.telephone],[userSettings.email]])
  .setFontWeight("bold")
  .setFontColors([['black'],['black'],['black']])
  .setHorizontalAlignment("left");

  receiptSheet.getRange(1, 1).setFontSize(18);
  receiptSheet.getRange(7, 3).setHorizontalAlignment("center");
  receiptSheet.getRange(5, 2).setHorizontalAlignment("left");
}

function pushVendorReceipt(receiptSheet, items, auctioneerSheet){
  //add new rows for new items
  receiptSheet.insertRowsBefore(6, items.length);

  //copy over new items
  var columns = [2, 3, 6];
  var startRow = 6;
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
