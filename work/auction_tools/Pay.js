function pay(p){

  //references to current spreadsheet and auctioneer sheet
  var ss = SpreadsheetApp.getActive();
  var auctioneerSheet = ss.getSheetByName("Auction Sheet");

  //show sheet as changes are made
  auctioneerSheet.activate();

  //purchaser number to be searched for and the current purchaser number in the loop
  var purchaserNum = parseInt(p);
  var purchaserCur;
  //maximum sale number found and the current sale number in the loop
  var curSaleID;
  var maxSaleID = 0;
  //array for storing unpaid items belonging to the purchaser
  var unpaidItems = [];

  for (var row = 2; row <= auctioneerSheet.getMaxRows(); row++){

    purchaserCur = auctioneerSheet.getRange(row, 7).getValue();
    if (purchaserCur == purchaserNum){

      curSaleID = auctioneerSheet.getRange(row, 8);
      if (curSaleID.isBlank()){
        unpaidItems.push(row);
      }
      else if (curSaleID.getValue() > maxSaleID){
       maxSaleID = curSaleID.getValue();
      }

    }

  }

  //fill in new saleID for unpaid items
  var newSaleID = maxSaleID + 1;
  for (var i = 0; i < unpaidItems.length; i++){
    auctioneerSheet.getRange(unpaidItems[i], 8).setValue(newSaleID);
  }
}
