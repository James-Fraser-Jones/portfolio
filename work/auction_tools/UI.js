function createAddonMenu(){
  var menu = SpreadsheetApp.getUi().createAddonMenu();
  menu.addItem('Authorize User', 'authorizeUser');
  menu.addToUi();
}

function authorizedAddonMenu(){
  var menu = SpreadsheetApp.getUi().createAddonMenu();
  menu.addItem('New Auction Spreadsheet', 'newAuctionSpreadsheetDialog');
  menu.addSeparator();
  menu.addItem('Show Receipts Tool', 'showReceiptsTool');
  menu.addSeparator();
  menu.addItem('Create Purchaser Receipts', 'purchaserReceiptsDialog');
  menu.addItem('Create Vendor Receipts', 'vendorReceiptsDialog');
  menu.addSeparator();
  menu.addItem('Get Catalogue', 'catalogue');
  menu.addItem('Analytics', 'analytics');
  menu.addSeparator();
  menu.addItem('Import', 'import');
  menu.addSeparator();
  menu.addItem('User Settings', 'userSettingsDialog');
  /*
  menu.addSeparator();
  menu.addItem('Import Raw Data', 'rawDataImport');

  menu.addItem('Test Pay', 'testPay');
  menu.addItem('Test Unpaid', 'testUnpaid');
  menu.addItem('Test Vendor', 'testVendor');
  */
  menu.addToUi();
}

function newAuctionSpreadsheetDialog() {
  var html = HtmlService.createHtmlOutputFromFile('NewAuctionSpreadsheet');
  html.setWidth(220);
  html.setHeight(100);
  SpreadsheetApp.getUi().showModalDialog(html, 'New Auction Spreadsheet');
}

function showReceiptsTool() {
  var html = HtmlService.createHtmlOutputFromFile('ReceiptsTool');
  html.setWidth(230);
  html.setHeight(140);
  SpreadsheetApp.getUi().showModelessDialog(html, 'Receipts Tool');
}

function purchaserReceiptsDialog() {
  var html = HtmlService.createHtmlOutputFromFile('PurchaserReceipts');
  html.setWidth(210);
  html.setHeight(90);
  SpreadsheetApp.getUi().showModalDialog(html, 'Create Purchaser Receipts');
}

function vendorReceiptsDialog() {
  var html = HtmlService.createHtmlOutputFromFile('VendorReceipts');
  html.setWidth(210);
  html.setHeight(90);
  SpreadsheetApp.getUi().showModalDialog(html, 'Create Vendor Receipts');
}

function userSettingsDialog() {
  var html = HtmlService.createHtmlOutputFromFile('UserSettings');
  html.setWidth(210);
  html.setHeight(460);
  SpreadsheetApp.getUi().showModalDialog(html, 'User Settings');
}
